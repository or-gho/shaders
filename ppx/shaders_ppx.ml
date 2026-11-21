open Ppxlib
module B = Ast_builder.Default

(* ------------------------------------------------------------------ *)
(* 1. Helpers                                                         *)
(* ------------------------------------------------------------------ *)

let name_of_expr ~loc (e : expression) : string =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident s; _ } -> s
  | _ -> Location.raise_errorf ~loc "Expected an identifier"
;;

(* `vec3` in the payload  →  expression `Shaders_of_ocaml.vec3` *)
let typ_of_expr (e : expression) : expression =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident s; _ } ->
    let lid = { txt = Ldot (Lident "Shaders_of_ocaml", s); loc = e.pexp_loc } in
    B.pexp_ident ~loc:e.pexp_loc lid
  | _ -> Location.raise_errorf ~loc:e.pexp_loc "Expected a type identifier"
;;

(* ------------------------------------------------------------------ *)
(* 2. Intermediate representation — now carries `expression`          *)
(* ------------------------------------------------------------------ *)

type field =
  | Attribute of string * expression
  | Uniform of string * expression
  | Varying of string * expression
  | Body of expression

(* ------------------------------------------------------------------ *)
(* 2.5 Duplicate-name check                                           *)
(* ------------------------------------------------------------------ *)

let check_duplicates ~loc (fields : field list) : unit =
  let names =
    List.map
      (function
        | Attribute (n, _) | Uniform (n, _) | Varying (n, _) -> n
        | Body _ -> "")
      fields
    |> List.filter (fun s -> s <> "")
    |> List.sort compare
  in
  let rec loop = function
    | a :: b :: _ when a = b -> Location.raise_errorf ~loc "Duplicate shader field: %s" a
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop names
;;

(* ------------------------------------------------------------------ *)
(* 3. Parse payload items                                             *)
(* ------------------------------------------------------------------ *)

let parse_item ~loc (item : structure_item) : field option =
  match item.pstr_desc with
  | Pstr_eval (e, _attrs) ->
    (match e.pexp_desc with
     | Pexp_apply
         ( { pexp_desc = Pexp_ident { txt = Lident "attribute"; _ }; _ }
         , [ (Nolabel, name); (Nolabel, typ) ] ) ->
       Some (Attribute (name_of_expr ~loc:name.pexp_loc name, typ_of_expr typ))
     | Pexp_apply
         ( { pexp_desc = Pexp_ident { txt = Lident "uniform"; _ }; _ }
         , [ (Nolabel, name); (Nolabel, typ) ] ) ->
       Some (Uniform (name_of_expr ~loc name, typ_of_expr typ))
     | Pexp_apply
         ( { pexp_desc = Pexp_ident { txt = Lident "varying"; _ }; _ }
         , [ (Nolabel, name); (Nolabel, typ) ] ) ->
       Some (Varying (name_of_expr ~loc name, typ_of_expr typ))
     | Pexp_apply
         ({ pexp_desc = Pexp_ident { txt = Lident "body"; _ }; _ }, [ (Nolabel, body) ])
       -> Some (Body body)
     | _ -> Location.raise_errorf ~loc:e.pexp_loc "Unknown shader field")
  | _ ->
    Location.raise_errorf
      ~loc:item.pstr_loc
      "Expected `attribute`, `uniform`, `varying`, or `body`"
;;

(* ------------------------------------------------------------------ *)
(* 4. Build combinator chain                                          *)
(* ------------------------------------------------------------------ *)

let build ~loc (fields : field list) : expression =
  (* Fully-qualify the facade so the generated code does not require
     the user to have `open Shaders_of_ocaml`. *)
  check_duplicates ~loc fields;
  let init = [%expr Shaders_of_ocaml.Vertex.create] in
  let names =
    List.filter_map
      (function
        | Attribute (n, _) | Uniform (n, _) -> Some n
        | Varying _ | Body _ -> None)
      fields
  in
  let body_field =
    List.find_opt
      (function
        | Body _ -> true
        | _ -> false)
      fields
  in
  let chain =
    List.fold_left
      (fun acc -> function
         | Attribute (n, t) ->
           [%expr
             [%e acc]
             |> Shaders_of_ocaml.Vertex.with_attribute ([%e t], [%e B.estring ~loc n])]
         | Uniform (n, t) ->
           [%expr
             [%e acc]
             |> Shaders_of_ocaml.Vertex.with_uniform ([%e t], [%e B.estring ~loc n])]
         | Varying (n, t) ->
           [%expr
             [%e acc]
             |> Shaders_of_ocaml.Vertex.with_varying ([%e t], [%e B.estring ~loc n])]
         | Body _ -> acc)
      init
      fields
  in
  match body_field with
  | None -> chain
  | Some (Body body) ->
    let pat =
      List.fold_right (fun n acc -> [%pat? [%p B.pvar ~loc n], [%p acc]]) names [%pat? ()]
    in
    let lambda = B.pexp_fun ~loc Nolabel None pat body in
    [%expr [%e chain] |> Shaders_of_ocaml.Vertex.with_body [%e lambda]]
  | Some _ -> assert false
;;

(* ------------------------------------------------------------------ *)
(* 5. Extension expander                                              *)
(* ------------------------------------------------------------------ *)

let expand_vertex ~loc ~path:_ (items : structure) : expression =
  let fields = List.filter_map (parse_item ~loc) items in
  build ~loc fields
;;

(* ------------------------------------------------------------------ *)
(* 6. Register                                                        *)
(* ------------------------------------------------------------------ *)

let vertex_extension =
  Extension.declare
    "shader.vertex"
    Extension.Context.expression
    Ast_pattern.(pstr __)
    expand_vertex
;;

let () =
  Driver.register_transformation
    "shaders_ppx"
    ~rules:[ Context_free.Rule.extension vertex_extension ]
;;
