open Shaders_of_ocaml
(* open Shaders_of_ocaml.Types
open Shaders_of_ocaml.Expr
open Shaders_of_ocaml.Stmt *)

let rec type_name : type a. a Types.ty -> string = function
  | Float -> "float"
  | Int -> "int"
  | Bool -> "bool"
  | Vec2 -> "vec2"
  | Vec3 -> "vec3"
  | Vec4 -> "vec4"
  | Mat2 -> "mat2"
  | Mat3 -> "mat3"
  | Mat4 -> "mat4"
  | Sampler2D -> "sampler2D"

and expr : type a. a Expr.expr -> string = function
  | Float_const f -> Printf.sprintf "%.6f" f
  | Int_const i -> string_of_int i
  | Bool_const b -> if b then "true" else "false"
  | Var (_, name) -> name
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (expr a) (expr b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (expr a) (expr b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (expr a) (expr b)
  | Div (a, b) -> Printf.sprintf "(%s / %s)" (expr a) (expr b)
  | Vec2_const (a, b) -> Printf.sprintf "vec2(%s, %s)" (expr a) (expr b)
  | Vec3_const (a, b, c) -> Printf.sprintf "vec3(%s, %s, %s)" (expr a) (expr b) (expr c)
  | Vec4_const (a, b, c, d) ->
    Printf.sprintf "vec4(%s, %s, %s, %s)" (expr a) (expr b) (expr c) (expr d)
  | Vec4_of_vec3 (v, w) -> Printf.sprintf "vec4(%s, %s)" (expr v) (expr w)
  | Vec_add (a, b) -> Printf.sprintf "(%s + %s)" (expr a) (expr b)
  | Vec_scale (s, v) -> Printf.sprintf "(%s * %s)" (expr s) (expr v)
  | Mat_mul_vec (m, v) -> Printf.sprintf "(%s * %s)" (expr m) (expr v)
  | Dot (a, b) -> Printf.sprintf "dot(%s, %s)" (expr a) (expr b)
  | Normalize v -> Printf.sprintf "normalize(%s)" (expr v)
  | X v -> Printf.sprintf "(%s).x" (expr v)
  | Y v -> Printf.sprintf "(%s).y" (expr v)
  | Z v -> Printf.sprintf "(%s).z" (expr v)
  | W v -> Printf.sprintf "(%s).w" (expr v)

and stmt : type a. a Stmt.stmt -> string = function
  | Assign ((_, name), e) -> Printf.sprintf "%s = %s;" name (expr e)
  | Let (ty, name, e) -> Printf.sprintf "%s %s = %s;" (type_name ty) name (expr e)
  | If (c, t, e) ->
    let then_ = String.concat "\n  " (List.map stmt t) in
    let else_ =
      match e with
      | [] -> ""
      | _ -> "\nelse {\n  " ^ String.concat "\n  " (List.map stmt e) ^ "\n}"
    in
    Printf.sprintf "if (%s) {\n  %s%s}" (expr c) then_ else_
  | Return e -> Printf.sprintf "return %s;" (expr e)
  | Expr_stmt e -> Printf.sprintf "%s;" (expr e)
;;

(* src/backend_glsl/transpile.ml, continued *)

let uniform_decls us =
  List.map (fun (ty, name) -> Printf.sprintf "uniform %s %s;" (type_name ty) name) us
  |> String.concat ""
;;

let compile_vertex vs =
  let header = "#version 330 core\n" in
  let attrs =
    List.mapi
      (fun i (ty, name) ->
         Printf.sprintf "layout(location = %d) in %s %s;\n" i (type_name ty) name)
      vs.Shader.vs_attributes
    |> String.concat ""
  in
  let uniforms = uniform_decls vs.Shader.vs_uniforms in
  let varyings =
    List.map
      (fun (ty, name) -> Printf.sprintf "out %s %s;\n" (type_name ty) name)
      vs.Shader.vs_varyings
    |> String.concat ""
  in
  let body = List.map stmt vs.Shader.vs_body |> String.concat "\n  " in
  header ^ attrs ^ uniforms ^ varyings ^ "\nvoid main() {\n  " ^ body ^ "\n}\n"
;;

let compile_fragment fs =
  let header = "#version 330 core\n" in
  let uniforms = uniform_decls fs.Shader.fs_uniforms in
  let varyings =
    List.map
      (fun (ty, name) -> Printf.sprintf "in %s %s;\n" (type_name ty) name)
      fs.Shader.fs_varyings
    |> String.concat ""
  in
  let out_ty, out_name = fs.Shader.fs_output in
  let out_decl = Printf.sprintf "out %s %s;\n" (type_name out_ty) out_name in
  let body = List.map stmt fs.Shader.fs_body |> String.concat "\n  " in
  header ^ uniforms ^ varyings ^ out_decl ^ "\nvoid main() {\n  " ^ body ^ "\n}\n"
;;

(* src/backend_glsl/transpile.ml, continued *)

let compile_pair vs fs = compile_vertex vs, compile_fragment fs
