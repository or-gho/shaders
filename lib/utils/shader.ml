open Types

type vertex_stage
type fragment_stage

type any_typ = Any : 'a Types.t -> any_typ

let equal_any_typ (Any a) (Any b) =
  String.equal (Types.glsl_name a) (Types.glsl_name b)

let string_of_any_typ (Any a) = Types.glsl_name a

type ('stage, 'inputs, 'varyings, 'outputs) builder =
  { b_attrs : (any_typ * string) list
  ; b_unifs : (any_typ * string) list
  ; b_varys : (any_typ * string) list
  ; b_outs : (any_typ * string) list
  }

type ('stage, 'inputs, 'varyings, 'outputs) t =
  { attrs : (any_typ * string) list
  ; unifs : (any_typ * string) list
  ; varys : (any_typ * string) list
  ; outs : (any_typ * string) list
  ; body : unit Stmt.stmt list
  }

type ('stage, 'inputs, 'varyings, 'outputs) shader =
  ('stage, 'inputs, 'varyings, 'outputs) t

let empty_builder () = { b_attrs = []; b_unifs = []; b_varys = []; b_outs = [] }

let is_ident name =
  let n = String.length name in
  let letter c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
  in
  let alnum c = letter c || (c >= '0' && c <= '9') in
  n > 0
  && letter name.[0]
  && let rec loop i = i = n || (alnum name.[i] && loop (i + 1)) in
     loop 1
;;

let check_name kind name (b : (_, _, _, _) builder) =
  if not (is_ident name)
  then invalid_arg (Printf.sprintf "Shader: %S is not a valid identifier" name);
  if String.starts_with ~prefix:"gl_" name
  then invalid_arg (Printf.sprintf "Shader: %S is reserved" name);
  let existing = b.b_attrs @ b.b_unifs @ b.b_varys @ b.b_outs in
  if List.exists (fun (_, n) -> n = name) existing
  then invalid_arg (Printf.sprintf "Shader: duplicate %s %S" kind name)
;;

let create_vertex () = empty_builder ()
let create_fragment () = empty_builder ()

let with_attribute name ty b =
  check_name "attribute" name b;
  { b with b_attrs = (Any ty, name) :: b.b_attrs }, (ty, name)
;;

let with_uniform name ty b =
  check_name "uniform" name b;
  { b with b_unifs = (Any ty, name) :: b.b_unifs }, (ty, name)
;;

let with_varying name ty b =
  check_name "varying" name b;
  { b with b_varys = (Any ty, name) :: b.b_varys }, (ty, name)
;;

let with_output name ty b =
  check_name "output" name b;
  { b with b_outs = (Any ty, name) :: b.b_outs }, (ty, name)
;;

let sorted_varys varys =
  List.sort (fun (_, a) (_, b) -> String.compare a b) (List.rev varys)
;;

let vertex_body b main =
  let gl_position : vec4 Stmt.ref_ = Types.Vec4, "gl_Position" in
  { attrs = List.rev b.b_attrs
  ; unifs = List.rev b.b_unifs
  ; varys = sorted_varys b.b_varys
  ; outs = []
  ; body = main gl_position
  }
;;

let fragment_body b body =
  { attrs = []
  ; unifs = List.rev b.b_unifs
  ; varys = sorted_varys b.b_varys
  ; outs = List.rev b.b_outs
  ; body
  }
;;
