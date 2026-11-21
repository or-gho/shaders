(* ppx/shaders_ppx.ml *)

open Ppxlib

(* ------------------------------------------------------------------ *)
(* Internal ty representation                                         *)
(* ------------------------------------------------------------------ *)

type ty =
  | Float | Int | UInt | Bool
  | Vec2 | Vec3 | Vec4
  | IVec2 | IVec3 | IVec4
  | UVec2 | UVec3 | UVec4
  | BVec2 | BVec3 | BVec4
  | Mat2 | Mat3 | Mat4

type expr =
  | EVar of string
  | EFloat of float
  | EInt of int
  | EBool of bool
  | ECall of string * expr list
  | EBinop of string * expr * expr

type stmt = SAssign of string * expr

type decl_kind = DAttribute | DUniform | DVarying | DOutput

type decl =
  { d_kind : decl_kind
  ; d_name : string
  ; d_ty : ty
  ; d_loc : Location.t
  }

type payload =
  { s_kind : [ `Vertex | `Fragment ]
  ; s_decls : decl list
  ; s_body : stmt list
  ; s_loc : Location.t
  }

let raise_err loc fmt = Location.raise_errorf ~loc fmt

(* ------------------------------------------------------------------ *)
(* Type names                                                         *)
(* ------------------------------------------------------------------ *)

let ty_of_name loc = function
  | "float" -> Float | "int" -> Int | "uint" -> UInt | "bool" -> Bool
  | "vec2" -> Vec2 | "vec3" -> Vec3 | "vec4" -> Vec4
  | "ivec2" -> IVec2 | "ivec3" -> IVec3 | "ivec4" -> IVec4
  | "uvec2" -> UVec2 | "uvec3" -> UVec3 | "uvec4" -> UVec4
  | "bvec2" -> BVec2 | "bvec3" -> BVec3 | "bvec4" -> BVec4
  | "mat2" -> Mat2 | "mat3" -> Mat3 | "mat4" -> Mat4
  | s -> raise_err loc "shader: unknown type %S" s
;;

(* Path into the Shaders_of_ocaml façade *)
let types_path = function
  | Float -> "Ty.float" | Int -> "Ty.int" | UInt -> "Ty.uint" | Bool -> "Ty.bool"
  | Vec2 -> "Ty.vec2" | Vec3 -> "Ty.vec3" | Vec4 -> "Ty.vec4"
  | IVec2 -> "Ty.ivec2" | IVec3 -> "Ty.ivec3" | IVec4 -> "Ty.ivec4"
  | UVec2 -> "Ty.uvec2" | UVec3 -> "Ty.uvec3" | UVec4 -> "Ty.uvec4"
  | BVec2 -> "Ty.bvec2" | BVec3 -> "Ty.bvec3" | BVec4 -> "Ty.bvec4"
  | Mat2 -> "Ty.mat2" | Mat3 -> "Ty.mat3" | Mat4 -> "Ty.mat4"
;;

(* ------------------------------------------------------------------ *)
(* Lexer                                                              *)
(* ------------------------------------------------------------------ *)

type token =
  | TIdent of string
  | TInt of int
  | TFloat of float
  | TColon | TSemi | TLBrace | TRBrace
  | TLParen | TRParen
  | TDot | TEq
  | TPlus | TMinus | TStar | TSlash
  | TComma
  | TEOF

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_letter c || is_digit c

let tokenize loc s =
  let n = String.length s in
  let toks = ref [] in
  let push t = toks := t :: !toks in
  let i = ref 0 in
  let peek () = if !i < n then Some s.[!i] else None in
  let advance () = incr i in
  while !i < n do
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') -> advance ()
    | Some ':' -> advance (); push TColon
    | Some ';' -> advance (); push TSemi
    | Some '{' -> advance (); push TLBrace
    | Some '}' -> advance (); push TRBrace
    | Some '(' -> advance (); push TLParen
    | Some ')' -> advance (); push TRParen
    | Some ',' -> advance (); push TComma
    | Some '.' -> advance (); push TDot
    | Some '=' -> advance (); push TEq
    | Some '+' -> advance (); push TPlus
    | Some '-' -> advance (); push TMinus
    | Some '*' -> advance (); push TStar
    | Some '/' -> advance (); push TSlash
    | Some c when is_letter c ->
      let start = !i in
      while (match peek () with Some c when is_alnum c -> true | _ -> false) do
        advance ()
      done;
      push (TIdent (String.sub s start (!i - start)))
    | Some c when is_digit c ->
      let start = !i in
      while (match peek () with Some c when is_digit c -> true | _ -> false) do
        advance ()
      done;
      let is_float =
        match peek () with
        | Some '.' ->
          let save = !i in
          advance ();
          (match peek () with
           | Some c when is_digit c ->
             while (match peek () with Some c when is_digit c -> true | _ -> false) do
               advance ()
             done;
             true
           | _ -> i := save; false)
        | _ -> false
      in
      let text = String.sub s start (!i - start) in
      push (if is_float then TFloat (float_of_string text)
            else TInt (int_of_string text))
    | Some c -> raise_err loc "shader: unexpected character %C" c
    | None -> ()
  done;
  push TEOF;
  List.rev !toks
;;

(* ------------------------------------------------------------------ *)
(* Parser                                                             *)
(* ------------------------------------------------------------------ *)

type stream = { mutable toks : token list; loc : Location.t }

let peek st = match st.toks with t :: _ -> t | [] -> TEOF
let next st = match st.toks with t :: r -> st.toks <- r; t | [] -> TEOF

let expect st t what =
  let got = next st in
  if got <> t then raise_err st.loc "shader: expected %s" what
;;

let parse_ty st =
  match next st with
  | TIdent s -> ty_of_name st.loc s
  | _ -> raise_err st.loc "shader: expected a type name"
;;

let parse_decl_kind st =
  match next st with
  | TIdent "attribute" -> DAttribute
  | TIdent "uniform" -> DUniform
  | TIdent "varying" -> DVarying
  | TIdent "output" -> DOutput
  | _ -> raise_err st.loc "shader: expected attribute/uniform/varying/output"
;;

let parse_ident st =
  match next st with
  | TIdent s -> s
  | _ -> raise_err st.loc "shader: expected an identifier"
;;

let rec parse_expr st =
  let lhs = parse_term st in
  let rec loop acc =
    match peek st with
    | TPlus -> ignore (next st); loop (EBinop ("+", acc, parse_term st))
    | TMinus -> ignore (next st); loop (EBinop ("-", acc, parse_term st))
    | _ -> acc
  in
  loop lhs

and parse_term st =
  let lhs = parse_primary st in
  let rec loop acc =
    match peek st with
    | TStar -> ignore (next st); loop (EBinop ("*", acc, parse_primary st))
    | TSlash -> ignore (next st); loop (EBinop ("/", acc, parse_primary st))
    | _ -> acc
  in
  loop lhs

and parse_primary st =
  match next st with
  | TInt i -> EInt i
  | TFloat f -> EFloat f
  | TIdent "true" -> EBool true
  | TIdent "false" -> EBool false
  | TIdent name ->
    (match peek st with
     | TLParen ->
       ignore (next st);
       let args = parse_args st in
       ECall (name, args)
     | _ -> EVar name)
  | _ -> raise_err st.loc "shader: expected an expression"

and parse_args st =
  match peek st with
  | TRParen -> ignore (next st); []
  | _ ->
    let a = parse_expr st in
    let rec loop acc =
      match peek st with
      | TRParen -> ignore (next st); List.rev acc
      | TComma -> ignore (next st); loop (parse_expr st :: acc)
      | _ -> raise_err st.loc "shader: expected ',' or ')'"
    in
    loop [ a ]
;;

let parse_stmt st =
  let name = parse_ident st in
  expect st TEq "=";
  let e = parse_expr st in
  expect st TSemi ";";
  SAssign (name, e)
;;

let parse_payload kind loc s =
  let st = { toks = tokenize loc s; loc } in
  let decls = ref [] in
  let rec decls_loop () =
    match peek st with
    | TIdent ("attribute" | "uniform" | "varying" | "output") ->
      let d_kind = parse_decl_kind st in
      let d_name = parse_ident st in
      expect st TColon ":";
      let d_ty = parse_ty st in
      expect st TSemi ";";
      decls := { d_kind; d_name; d_ty; d_loc = loc } :: !decls;
      decls_loop ()
    | _ -> ()
  in
  decls_loop ();
  expect st (TIdent "void") "void";
  expect st TLBrace "{";
  let body = ref [] in
  let rec body_loop () =
    match peek st with
    | TRBrace -> ignore (next st)
    | _ -> body := parse_stmt st :: !body; body_loop ()
  in
  body_loop ();
  expect st TEOF "end of payload";
  { s_kind = kind; s_decls = List.rev !decls; s_body = List.rev !body; s_loc = loc }
;;

(* ------------------------------------------------------------------ *)
(* Checker                                                            *)
(* ------------------------------------------------------------------ *)

type binding = { b_ty : ty; b_kind : decl_kind }

let name_ok name =
  let n = String.length name in
  n > 0 && is_letter name.[0]
  && (let rec loop i = i = n || (is_alnum name.[i] && loop (i + 1)) in loop 1)
;;

let build_table (payload : payload) =
  let tbl = Hashtbl.create 16 in
  if payload.s_kind = `Vertex then
    Hashtbl.add tbl "gl_Position" { b_ty = Vec4; b_kind = DOutput };
  List.iter
    (fun d ->
       if not (name_ok d.d_name) then
         raise_err d.d_loc "shader: %S is not a valid identifier" d.d_name;
       if String.starts_with ~prefix:"gl_" d.d_name
          && not (payload.s_kind = `Vertex && d.d_name = "gl_Position")
       then raise_err d.d_loc "shader: %S is reserved" d.d_name;
       if Hashtbl.mem tbl d.d_name then
         raise_err d.d_loc "shader: duplicate declaration %S" d.d_name;
       (match d.d_kind, payload.s_kind with
        | DAttribute, `Fragment ->
          raise_err d.d_loc "shader: 'attribute' is only valid in a vertex shader"
        | DOutput, `Vertex ->
          raise_err d.d_loc "shader: 'output' is only valid in a fragment shader"
        | _ -> ());
       Hashtbl.add tbl d.d_name { b_ty = d.d_ty; b_kind = d.d_kind })
    payload.s_decls;
  tbl
;;

(* ------------------------------------------------------------------ *)
(* AST builders                                                       *)
(* ------------------------------------------------------------------ *)

type typed = ty * expression

let e_fun name =
  let loc = Location.none in
  match String.index_opt name '.' with
  | None ->
    Ast_builder.Default.pexp_ident ~loc { txt = Lident name; loc }
  | Some i ->
    let m = String.sub name 0 i in
    let f = String.sub name (i + 1) (String.length name - i - 1) in
    Ast_builder.Default.pexp_ident ~loc { txt = Ldot (Lident m, f); loc }
;;

let e_apply fn args =
  Ast_builder.Default.pexp_apply ~loc:Location.none fn
    (List.map (fun a -> Nolabel, a) args)
;;

let e_ref loc ty name =
  let ty_e = e_fun (types_path ty) in
  let str = Ast_builder.Default.estring ~loc name in
  Ast_builder.Default.pexp_tuple ~loc [ ty_e; str ]
;;

let e_var loc ty name =
  e_apply (e_fun "S.read")
    [ Ast_builder.Default.evar ~loc name ]

let e_float f =
  Ast_builder.Default.pexp_constant ~loc:Location.none
    (Pconst_float (Printf.sprintf "%h" f, None))
;;

let e_int i =
  Ast_builder.Default.pexp_constant ~loc:Location.none
    (Pconst_integer (string_of_int i, None))
;;

let e_bool b =
  Ast_builder.Default.pexp_construct ~loc:Location.none
    { txt = Lident (if b then "true" else "false"); loc = Location.none }
    None
;;

(* Map a user-written call like `vec4(x, 1.0)` to a façade function. *)
let fn_for_call _loc name arg_tys =
  match name, arg_tys with
  | "vec2", [ Float; Float ] -> Some "E.vec2"
  | "vec3", [ Float; Float; Float ] -> Some "E.vec3"
  | "vec4", [ Float; Float; Float; Float ] -> Some "E.vec4"
  | "vec4", [ Vec3; Float ] -> Some "E.vec4_of_vec3"
  | "abs", [ Float ] -> Some "E.abs_f"
  | "floor", [ Float ] -> Some "E.floor_f"
  | "fract", [ Float ] -> Some "E.fract_f"
  | "sqrt", [ Float ] -> Some "E.sqrt"
  | "pow", [ Float; Float ] -> Some "E.pow"
  | "sin", [ Float ] -> Some "E.sin"
  | "cos", [ Float ] -> Some "E.cos"
  | "min", [ Float; Float ] -> Some "E.min_f"
  | "max", [ Float; Float ] -> Some "E.max_f"
  | "clamp", [ Float; Float; Float ] -> Some "E.clamp_f"
  | "mix", [ Float; Float; Float ] -> Some "E.mix_f"
  | "step", [ Float; Float ] -> Some "E.step_f"
  | "smoothstep", [ Float; Float; Float ] -> Some "E.smoothstep_f"
  | "normalize", [ Vec3 ] -> Some "E.normalize3"
  | "dot", [ Vec3; Vec3 ] -> Some "E.dot3"
  | "cross", [ Vec3; Vec3 ] -> Some "E.cross3"
  | "length", [ Vec3 ] -> Some "E.length3"
  | "distance", [ Vec3; Vec3 ] -> Some "E.distance3"
  | "reflect", [ Vec3; Vec3 ] -> Some "E.reflect3"
  | "float", [ Int ] -> Some "E.float_of_int"
  | "int", [ Float ] -> Some "E.int_of_float"
  | _ -> None
;;

let result_ty_of_fn = function
  | "E.vec2" -> Vec2
  | "E.vec3" | "E.normalize3" | "E.cross3" | "E.reflect3" -> Vec3
  | "E.vec4" | "E.vec4_of_vec3" | "E.mat4_mul_vec4" -> Vec4
  | "E.abs_f" | "E.floor_f" | "E.fract_f" | "E.sqrt" | "E.pow"
  | "E.sin" | "E.cos" | "E.min_f" | "E.max_f" | "E.clamp_f"
  | "E.mix_f" | "E.step_f" | "E.smoothstep_f"
  | "E.dot3" | "E.length3" | "E.distance3"
  | "E.float_of_int"
  | "E.add_f" | "E.sub_f" | "E.mul_f" | "E.div_f" -> Float
  | "E.add_i" | "E.sub_i" | "E.mul_i" | "E.div_i" | "E.int_of_float" -> Int
  | "E.add_v2" -> Vec2
  | "E.add_v3" | "E.sub_v3" -> Vec3
  | "E.add_v4" -> Vec4
  | _ -> raise (Invalid_argument "unknown façade function")
;;

let rec type_expr ?(handles = Hashtbl.create 0) ?(position_handle = "gl_position") tbl (e : expr) : typed =
  match e with
  | EFloat f -> Float, e_apply (e_fun "E.float") [ e_float f ]
  | EInt i -> Int, e_apply (e_fun "E.int") [ e_int i ]
  | EBool b -> Bool, e_apply (e_fun "E.bool") [ e_bool b ]
  | EVar name ->
    (match Hashtbl.find_opt tbl name with
     | Some { b_ty; _ } ->
       let handle = if name = "gl_Position" then position_handle
         else Option.value (Hashtbl.find_opt handles name) ~default:name in
       b_ty, e_var Location.none b_ty handle
     | None -> raise (Invalid_argument ("shader: unbound identifier " ^ name)))
  | ECall (name, args) ->
    let typed_args = List.map (type_expr ~handles ~position_handle tbl) args in
    let arg_tys = List.map fst typed_args in
    let arg_exprs = List.map snd typed_args in
    (match fn_for_call Location.none name arg_tys with
     | Some fn -> result_ty_of_fn fn, e_apply (e_fun fn) arg_exprs
     | None ->
       raise (Invalid_argument
                (Printf.sprintf "shader: unsupported call %s/%d" name
                   (List.length arg_tys))))
  | EBinop (op, a, b) ->
    let ta, ea = type_expr ~handles ~position_handle tbl a in
    let tb, eb = type_expr ~handles ~position_handle tbl b in
    let args = [ ea; eb ] in
    (match op, ta, tb with
     | "*", Mat4, Vec4 -> Vec4, e_apply (e_fun "E.mat4_mul_vec4") args
     | "+", Float, Float -> Float, e_apply (e_fun "E.add_f") args
     | "-", Float, Float -> Float, e_apply (e_fun "E.sub_f") args
     | "*", Float, Float -> Float, e_apply (e_fun "E.mul_f") args
     | "/", Float, Float -> Float, e_apply (e_fun "E.div_f") args
     | "+", Int, Int -> Int, e_apply (e_fun "E.add_i") args
     | "-", Int, Int -> Int, e_apply (e_fun "E.sub_i") args
     | "*", Int, Int -> Int, e_apply (e_fun "E.mul_i") args
     | "/", Int, Int -> Int, e_apply (e_fun "E.div_i") args
     | "+", Vec2, Vec2 -> Vec2, e_apply (e_fun "E.add_v2") args
     | "+", Vec3, Vec3 -> Vec3, e_apply (e_fun "E.add_v3") args
     | "-", Vec3, Vec3 -> Vec3, e_apply (e_fun "E.sub_v3") args
     | "+", Vec4, Vec4 -> Vec4, e_apply (e_fun "E.add_v4") args
     | _ ->
       raise (Invalid_argument
                (Printf.sprintf "shader: unsupported operator %s on these types" op)))
;;

(* ------------------------------------------------------------------ *)
(* Emitter                                                            *)
(* ------------------------------------------------------------------ *)

let stage_qualify kind s =
  match kind with
  | `Vertex -> "Vertex." ^ s
  | `Fragment -> "Fragment." ^ s
;;

let emit_decls loc kind decls handles initial_acc body_emit =
  let used = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.replace used d.d_name ()) decls;
  Hashtbl.replace used initial_acc ();
  let fresh () =
    let rec loop n = let x = "__shader_builder_" ^ string_of_int n in
      if Hashtbl.mem used x then loop (n + 1) else (Hashtbl.add used x (); x)
    in loop 0
  in
  let rec build acc = function
    | [] -> body_emit acc
    | d :: rest ->
      let fn =
        match d.d_kind with
        | DAttribute -> "Vertex.attribute"
        | DUniform -> stage_qualify kind "uniform"
        | DVarying -> stage_qualify kind "varying"
        | DOutput -> "Fragment.output"
      in
      let name_e = Ast_builder.Default.estring ~loc d.d_name in
      let ty_e = e_fun (types_path d.d_ty) in
      let call = e_apply (e_fun fn)
          [ name_e; ty_e; Ast_builder.Default.evar ~loc acc ] in
      let new_acc = fresh () in
      let pat =
        Ast_builder.Default.ppat_tuple ~loc
          [ Ast_builder.Default.pvar ~loc new_acc
          ; Ast_builder.Default.pvar ~loc (Hashtbl.find handles d.d_name) ]
      in
      Ast_builder.Default.pexp_let ~loc Asttypes.Nonrecursive
        [ Ast_builder.Default.value_binding ~loc ~pat ~expr:call ]
        (build new_acc rest)
  in
  build initial_acc decls
;;

let emit_assign loc kind tbl handles position_handle (SAssign (lhs_name, rhs)) =
  match Hashtbl.find_opt tbl lhs_name with
  | None -> raise_err loc "shader: assignment to undeclared %S" lhs_name
  | Some { b_ty; b_kind } ->
    (match b_kind, kind with
     | DUniform, _ -> raise_err loc "shader: cannot assign to uniform %S" lhs_name
     | DAttribute, _ -> raise_err loc "shader: cannot assign to attribute %S" lhs_name
     | DVarying, `Fragment -> raise_err loc "shader: cannot assign to fragment varying %S" lhs_name
     | DVarying, `Vertex -> ()
     | DOutput, _ -> ());
    let _, rhs_e = type_expr ~handles ~position_handle tbl rhs in
    let lhs =
      if lhs_name = "gl_Position" then
        Ast_builder.Default.evar ~loc position_handle
      else
        Ast_builder.Default.evar ~loc (Hashtbl.find handles lhs_name)
    in
    e_apply (e_fun "S.assign") [ lhs; rhs_e ]
;;

let emit_payload (p : payload) =
  let loc = p.s_loc in
  let tbl = build_table p in
  let handles = Hashtbl.create 16 in
  List.iteri (fun i d -> Hashtbl.add handles d.d_name ("__shader_value_" ^ string_of_int i)) p.s_decls;
  let position_handle = "__shader_position" in
  let initial_acc = "__shader_builder_start" in
  let body_exprs = List.map (emit_assign loc p.s_kind tbl handles position_handle) p.s_body in
  let body = Ast_builder.Default.elist ~loc body_exprs in
  let finalize acc =
    match p.s_kind with
    | `Fragment ->
      e_apply (e_fun "Fragment.body")
        [ Ast_builder.Default.evar ~loc acc; body ]
    | `Vertex ->
      let lam =
        Ast_builder.Default.pexp_fun ~loc Nolabel None
          (Ast_builder.Default.pvar ~loc position_handle) body
      in
      e_apply (e_fun "Vertex.body")
        [ Ast_builder.Default.evar ~loc acc; lam ]
  in
  let create =
    e_apply
      (e_fun (match p.s_kind with
              | `Vertex -> "Vertex.create"
              | `Fragment -> "Fragment.create"))
      [ Ast_builder.Default.eunit ~loc ]
  in
  Ast_builder.Default.pexp_let ~loc Asttypes.Nonrecursive
    [ Ast_builder.Default.value_binding ~loc
        ~pat:(Ast_builder.Default.pvar ~loc initial_acc)
        ~expr:create ]
    (emit_decls loc p.s_kind p.s_decls handles initial_acc finalize)
;;

(* ------------------------------------------------------------------ *)
(* Extension entry points                                             *)
(* ------------------------------------------------------------------ *)

let expand ~kind ~ctxt payload_str =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let payload = parse_payload kind loc payload_str in
  emit_payload payload
;;

let extension_vertex =
  Extension.V3.declare "shader.vertex" Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun ~ctxt s -> expand ~kind:`Vertex ~ctxt s)
;;

let extension_fragment =
  Extension.V3.declare "shader.fragment" Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun ~ctxt s -> expand ~kind:`Fragment ~ctxt s)
;;

let () =
  Driver.register_transformation "shaders_ppx"
    ~rules:
      [ Context_free.Rule.extension extension_vertex
      ; Context_free.Rule.extension extension_fragment
      ]
;;
