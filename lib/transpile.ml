open Shaders_utils
open Types
open Shader

(* ================================================================== *)
(* Shared                                                              *)
(* ================================================================== *)

type backend =
  | GLSL
  | WGSL

let float_lit (f : float) : string =
  match classify_float f with
  | FP_nan -> "0.0"
  | FP_infinite -> if f > 0.0 then "1e38" else "-1e38"
  | _ ->
    let s = string_of_float f in
    let len = String.length s in
    if len > 0 && s.[len - 1] = '.' then s ^ "0" else s
;;

let pad n = String.make n ' '

(* GLSL + WGSL share precedence levels because the operators coincide *)
let p_low = 0
let p_or = 3
let p_and = 4
let p_eq = 5
let p_rel = 6
let p_add = 7
let p_mul = 8
let p_unary = 9
let p_post = 10
let paren ~prec ~min_prec s = if prec < min_prec then "(" ^ s ^ ")" else s
let filter_varys varys = List.filter (fun (_, name) -> name <> "gl_Position") varys

(* ================================================================== *)
(* GLSL backend                                                        *)
(* ================================================================== *)

module Glsl = struct
  let rec expr : type a. ?min_prec:int -> a Expr.t -> string =
    fun ?(min_prec = p_low) e ->
    let go : type b. int -> b Expr.t -> string = fun p x -> expr ~min_prec:p x in
    match e with
    | Expr.Float_const f -> float_lit f
    | Expr.Int_const i -> string_of_int i
    | Expr.UInt_const i -> string_of_int i ^ "u"
    | Expr.Bool_const b -> if b then "true" else "false"
    | Expr.Var (_, name) -> name
    | Expr.Fadd (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.Fsub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.Fmul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.Fdiv (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " / " ^ go (p_mul + 1) b)
    | Expr.Fneg a -> paren ~prec:p_unary ~min_prec ("-" ^ go (p_unary + 1) a)
    | Expr.Iadd (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.Isub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.Imul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.Idiv (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " / " ^ go (p_mul + 1) b)
    | Expr.Ineg a -> paren ~prec:p_unary ~min_prec ("-" ^ go (p_unary + 1) a)
    | Expr.Flt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " < " ^ go (p_rel + 1) b)
    | Expr.Fle (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " <= " ^ go (p_rel + 1) b)
    | Expr.Fgt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " > " ^ go (p_rel + 1) b)
    | Expr.Fge (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " >= " ^ go (p_rel + 1) b)
    | Expr.Feq_f (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " == " ^ go (p_eq + 1) b)
    | Expr.Fne_f (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " != " ^ go (p_eq + 1) b)
    | Expr.Ilt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " < " ^ go (p_rel + 1) b)
    | Expr.Ile (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " <= " ^ go (p_rel + 1) b)
    | Expr.Igt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " > " ^ go (p_rel + 1) b)
    | Expr.Ige (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " >= " ^ go (p_rel + 1) b)
    | Expr.Ieq (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " == " ^ go (p_eq + 1) b)
    | Expr.Ine (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " != " ^ go (p_eq + 1) b)
    | Expr.And (a, b) ->
      paren ~prec:p_and ~min_prec (go (p_and + 1) a ^ " && " ^ go (p_and + 1) b)
    | Expr.Or (a, b) ->
      paren ~prec:p_or ~min_prec (go (p_or + 1) a ^ " || " ^ go (p_or + 1) b)
    | Expr.Not a -> paren ~prec:p_unary ~min_prec ("!" ^ go (p_unary + 1) a)
    | Expr.Vec2_of_f a -> "vec2(" ^ go p_low a ^ ")"
    | Expr.Vec2_of_ff (a, b) -> "vec2(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Vec3_of_f a -> "vec3(" ^ go p_low a ^ ")"
    | Expr.Vec3_of_fff (a, b, c) ->
      "vec3(" ^ go p_low a ^ ", " ^ go p_low b ^ ", " ^ go p_low c ^ ")"
    | Expr.Vec3_of_v2_f (v, f) -> "vec3(" ^ go p_low v ^ ", " ^ go p_low f ^ ")"
    | Expr.Vec4_of_f a -> "vec4(" ^ go p_low a ^ ")"
    | Expr.Vec4_of_ffff (a, b, c, d) ->
      "vec4("
      ^ go p_low a
      ^ ", "
      ^ go p_low b
      ^ ", "
      ^ go p_low c
      ^ ", "
      ^ go p_low d
      ^ ")"
    | Expr.Vec4_of_v3_f (v, f) -> "vec4(" ^ go p_low v ^ ", " ^ go p_low f ^ ")"
    | Expr.Vec4_of_v2_ff (v, f, g) ->
      "vec4(" ^ go p_low v ^ ", " ^ go p_low f ^ ", " ^ go p_low g ^ ")"
    | Expr.V2add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V2sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V2mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V2scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V3add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V3sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V3mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V3scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V4add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V4sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V4mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V4scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V2x v -> go p_post v ^ ".x"
    | Expr.V2y v -> go p_post v ^ ".y"
    | Expr.V3x v -> go p_post v ^ ".x"
    | Expr.V3y v -> go p_post v ^ ".y"
    | Expr.V3z v -> go p_post v ^ ".z"
    | Expr.V4x v -> go p_post v ^ ".x"
    | Expr.V4y v -> go p_post v ^ ".y"
    | Expr.V4z v -> go p_post v ^ ".z"
    | Expr.V4w v -> go p_post v ^ ".w"
    | Expr.V4xyz v -> go p_post v ^ ".xyz"
    | Expr.V4xy v -> go p_post v ^ ".xy"
    | Expr.Dot_v2 (a, b) -> "dot(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Dot_v3 (a, b) -> "dot(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Dot_v4 (a, b) -> "dot(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Cross_v3 (a, b) -> "cross(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Length_v2 v -> "length(" ^ go p_low v ^ ")"
    | Expr.Length_v3 v -> "length(" ^ go p_low v ^ ")"
    | Expr.Length_v4 v -> "length(" ^ go p_low v ^ ")"
    | Expr.Distance_v3 (a, b) -> "distance(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Normalize_v2 v -> "normalize(" ^ go p_low v ^ ")"
    | Expr.Normalize_v3 v -> "normalize(" ^ go p_low v ^ ")"
    | Expr.Normalize_v4 v -> "normalize(" ^ go p_low v ^ ")"
    | Expr.Reflect_v3 (i, n) -> "reflect(" ^ go p_low i ^ ", " ^ go p_low n ^ ")"
    | Expr.Refract_v3 (i, n, e) ->
      "refract(" ^ go p_low i ^ ", " ^ go p_low n ^ ", " ^ go p_low e ^ ")"
    | Expr.Mat2_mul_vec2 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat3_mul_vec3 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat4_mul_vec4 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat2_mul_mat2 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Mat3_mul_mat3 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Mat4_mul_mat4 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Transpose_m2 m -> "transpose(" ^ go p_low m ^ ")"
    | Expr.Transpose_m3 m -> "transpose(" ^ go p_low m ^ ")"
    | Expr.Transpose_m4 m -> "transpose(" ^ go p_low m ^ ")"
    | Expr.Inverse_m4 m -> "inverse(" ^ go p_low m ^ ")"
    | Expr.Determinant_m4 m -> "determinant(" ^ go p_low m ^ ")"
    | Expr.Abs_f a -> "abs(" ^ go p_low a ^ ")"
    | Expr.Sign_f a -> "sign(" ^ go p_low a ^ ")"
    | Expr.Floor a -> "floor(" ^ go p_low a ^ ")"
    | Expr.Ceil a -> "ceil(" ^ go p_low a ^ ")"
    | Expr.Fract a -> "fract(" ^ go p_low a ^ ")"
    | Expr.Sqrt a -> "sqrt(" ^ go p_low a ^ ")"
    | Expr.InverseSqrt a -> "inversesqrt(" ^ go p_low a ^ ")"
    | Expr.Pow (a, b) -> "pow(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Exp a -> "exp(" ^ go p_low a ^ ")"
    | Expr.Log a -> "log(" ^ go p_low a ^ ")"
    | Expr.Sin a -> "sin(" ^ go p_low a ^ ")"
    | Expr.Cos a -> "cos(" ^ go p_low a ^ ")"
    | Expr.Tan a -> "tan(" ^ go p_low a ^ ")"
    | Expr.Asin a -> "asin(" ^ go p_low a ^ ")"
    | Expr.Acos a -> "acos(" ^ go p_low a ^ ")"
    | Expr.Atan a -> "atan(" ^ go p_low a ^ ")"
    | Expr.Atan2 (y, x) -> "atan(" ^ go p_low y ^ ", " ^ go p_low x ^ ")"
    | Expr.Min_f (a, b) -> "min(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Max_f (a, b) -> "max(" ^ go p_low a ^ ", " ^ go p_low b ^ ")"
    | Expr.Clamp_f (x, lo, hi) ->
      "clamp(" ^ go p_low x ^ ", " ^ go p_low lo ^ ", " ^ go p_low hi ^ ")"
    | Expr.Mix_f (a, b, t) ->
      "mix(" ^ go p_low a ^ ", " ^ go p_low b ^ ", " ^ go p_low t ^ ")"
    | Expr.Step_f (e, x) -> "step(" ^ go p_low e ^ ", " ^ go p_low x ^ ")"
    | Expr.Smoothstep_f (e0, e1, x) ->
      "smoothstep(" ^ go p_low e0 ^ ", " ^ go p_low e1 ^ ", " ^ go p_low x ^ ")"
    | Expr.Float_of_int i -> "float(" ^ go p_low i ^ ")"
    | Expr.Int_of_float f -> "int(" ^ go p_low f ^ ")"
    | Expr.UInt_of_int i -> "uint(" ^ go p_low i ^ ")"
    | Expr.Int_of_uint u -> "int(" ^ go p_low u ^ ")"
    | Expr.Texture2D (s, uv) -> "texture(" ^ go p_low s ^ ", " ^ go p_low uv ^ ")"
    | Expr.Texture2D_lod (s, uv, l) ->
      "textureLod(" ^ go p_low s ^ ", " ^ go p_low uv ^ ", " ^ go p_low l ^ ")"
    | Expr.Texture2DShadow (s, uv) -> "texture(" ^ go p_low s ^ ", " ^ go p_low uv ^ ")"
    | Expr.TextureCube (s, d) -> "texture(" ^ go p_low s ^ ", " ^ go p_low d ^ ")"
    | Expr.TexelFetch2D (s, c, l) ->
      "texelFetch(" ^ go p_low s ^ ", " ^ go p_low c ^ ", " ^ go p_low l ^ ")"
  ;;

  let rec stmt : type a. indent:int -> a Stmt.stmt -> string =
    fun ~indent s ->
    let p1 = pad indent
    and p2 = pad (indent + 2) in
    let seq n ss = String.concat "" (List.map (stmt ~indent:n) ss) in
    match s with
    | Stmt.Assign ((_, name), e) -> p1 ^ name ^ " = " ^ expr e ^ ";\n"
    | Stmt.Let (typ, name, e) ->
      p1 ^ Types.glsl_name typ ^ " " ^ name ^ " = " ^ expr e ^ ";\n"
    | Stmt.Decl_var (typ, name, e) ->
      p1 ^ Types.glsl_name typ ^ " " ^ name ^ " = " ^ expr e ^ ";\n"
    | Stmt.Expr_stmt e -> p1 ^ expr e ^ ";\n"
    | Stmt.If (c, thn, els) ->
      if els = []
      then p1 ^ "if (" ^ expr c ^ ") {\n" ^ seq (indent + 2) thn ^ p1 ^ "}\n"
      else
        p1
        ^ "if ("
        ^ expr c
        ^ ") {\n"
        ^ seq (indent + 2) thn
        ^ p1
        ^ "} else {\n"
        ^ seq (indent + 2) els
        ^ p1
        ^ "}\n"
    | Stmt.For ((typ, i), start, stop, body) ->
      p1
      ^ "for ("
      ^ Types.glsl_name typ
      ^ " "
      ^ i
      ^ " = "
      ^ expr start
      ^ "; "
      ^ i
      ^ " < "
      ^ expr stop
      ^ "; "
      ^ i
      ^ "++) {\n"
      ^ seq (indent + 2) body
      ^ p1
      ^ "}\n"
    | Stmt.While (c, body) ->
      p1 ^ "while (" ^ expr c ^ ") {\n" ^ seq (indent + 2) body ^ p1 ^ "}\n"
    | Stmt.DoWhile (body, c) ->
      p1 ^ "do {\n" ^ seq (indent + 2) body ^ p1 ^ "} while (" ^ expr c ^ ");\n"
    | Stmt.Switch (e, cases, default) ->
      let case_s (v, body) =
        p2 ^ "case " ^ string_of_int v ^ ":\n" ^ seq (indent + 4) body ^ p2 ^ "  break;\n"
      in
      let dflt =
        match default with
        | None -> ""
        | Some body -> p2 ^ "default:\n" ^ seq (indent + 4) body ^ p2 ^ "  break;\n"
      in
      p1
      ^ "switch ("
      ^ expr e
      ^ ") {\n"
      ^ String.concat "" (List.map case_s cases)
      ^ dflt
      ^ p1
      ^ "}\n"
    | Stmt.Break -> p1 ^ "break;\n"
    | Stmt.Continue -> p1 ^ "continue;\n"
    | Stmt.Discard -> p1 ^ "discard;\n"
    | Stmt.Block body -> p1 ^ "{\n" ^ seq (indent + 2) body ^ p1 ^ "}\n"
    | Stmt.Return e -> p1 ^ "return " ^ expr e ^ ";\n"
  ;;

  (* ----- emit whole vertex shader ----- *)
  let emit_vertex (sh : (Shader.vertex_stage, _, _, _) Shader.t) : string =
    let b = Buffer.create 4096 in
    let addf fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    addf "#version 330 core\n\n";
    List.iteri
      (fun i (Any typ, name) ->
         addf "layout(location = %d) in %s %s;\n" i (Types.glsl_name typ) name)
      sh.attrs;
    List.iter
      (fun (Any typ, name) -> addf "uniform %s %s;\n" (Types.glsl_name typ) name)
      sh.unifs;
    List.iter
      (fun (Any typ, name) -> addf "out %s %s;\n" (Types.glsl_name typ) name)
      (filter_varys sh.varys);
    addf "\nvoid main() {\n";
    List.iter (fun s -> Buffer.add_string b (stmt ~indent:2 s)) sh.body;
    addf "}\n";
    Buffer.contents b
  ;;

  (* ----- emit whole fragment shader ----- *)
  let emit_fragment (sh : (Shader.fragment_stage, _, _, _) Shader.t) : string =
    let b = Buffer.create 4096 in
    let addf fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    addf "#version 330 core\n\n";
    List.iter
      (fun (Any typ, name) -> addf "in %s %s;\n" (Types.glsl_name typ) name)
      (filter_varys sh.varys);
    List.iter
      (fun (Any typ, name) -> addf "uniform %s %s;\n" (Types.glsl_name typ) name)
      sh.unifs;
    List.iteri
      (fun i (Any typ, name) ->
         addf "layout(location = %d) out %s %s;\n" i (Types.glsl_name typ) name)
      sh.outs;
    addf "\nvoid main() {\n";
    List.iter (fun s -> Buffer.add_string b (stmt ~indent:2 s)) sh.body;
    addf "}\n";
    Buffer.contents b
  ;;
end

(* ================================================================== *)
(* WGSL backend                                                        *)
(* ================================================================== *)

module Wgsl = struct
  type scope =
    { attrs : string list
    ; unifs : string list (* non-sampler uniforms inside struct Uniforms *)
    ; varys : string list
    ; outs : string list
    ; is_vertex : bool
    }

  let is_sampler (Any t) =
    match t with
    | Types.Sampler2D
    | Types.Sampler3D
    | Types.SamplerCube
    | Types.Sampler2DArray
    | Types.Sampler2DShadow
    | Types.ISampler2D
    | Types.USampler2D -> true
    | _ -> false
  ;;

  let sampler_kind (Any t) =
    match t with
    | Types.Sampler2DShadow -> Some "sampler_comparison"
    | Types.Sampler2D | Types.Sampler3D | Types.SamplerCube | Types.Sampler2DArray ->
      Some "sampler"
    | _ -> None
  ;;

  let ref_of scope name =
    if name = "gl_Position"
    then "output.position"
    else if List.mem name scope.attrs
    then "input." ^ name
    else if List.mem name scope.unifs
    then "uniforms." ^ name
    else if List.mem name scope.varys
    then if scope.is_vertex then "output." ^ name else "input." ^ name
    else if List.mem name scope.outs
    then "output." ^ name
    else name
  ;;

  let sampler_name : type a. a Expr.t -> string = function
    | Expr.Var (_, name) -> name ^ "_sampler"
    | _ -> "samp"
  ;;

  let rec uses_inverse_expr : type a. a Expr.t -> bool = function
    | Expr.Inverse_m4 _ -> true
    | Expr.Float_const _
    | Expr.Int_const _
    | Expr.UInt_const _
    | Expr.Bool_const _
    | Expr.Var _ -> false
    | Expr.Fneg a -> uses_inverse_expr a
    | Expr.Ineg a -> uses_inverse_expr a
    | Expr.Not a -> uses_inverse_expr a
    | Expr.Vec2_of_f a -> uses_inverse_expr a
    | Expr.Vec3_of_f a -> uses_inverse_expr a
    | Expr.Vec4_of_f a -> uses_inverse_expr a
    | Expr.V2x a -> uses_inverse_expr a
    | Expr.V2y a -> uses_inverse_expr a
    | Expr.V3x a -> uses_inverse_expr a
    | Expr.V3y a -> uses_inverse_expr a
    | Expr.V3z a -> uses_inverse_expr a
    | Expr.V4x a -> uses_inverse_expr a
    | Expr.V4y a -> uses_inverse_expr a
    | Expr.V4z a -> uses_inverse_expr a
    | Expr.V4w a -> uses_inverse_expr a
    | Expr.V4xyz a -> uses_inverse_expr a
    | Expr.V4xy a -> uses_inverse_expr a
    | Expr.Length_v2 a -> uses_inverse_expr a
    | Expr.Length_v3 a -> uses_inverse_expr a
    | Expr.Length_v4 a -> uses_inverse_expr a
    | Expr.Normalize_v2 a -> uses_inverse_expr a
    | Expr.Normalize_v3 a -> uses_inverse_expr a
    | Expr.Normalize_v4 a -> uses_inverse_expr a
    | Expr.Abs_f a -> uses_inverse_expr a
    | Expr.Sign_f a -> uses_inverse_expr a
    | Expr.Floor a -> uses_inverse_expr a
    | Expr.Ceil a -> uses_inverse_expr a
    | Expr.Fract a -> uses_inverse_expr a
    | Expr.Sqrt a -> uses_inverse_expr a
    | Expr.InverseSqrt a -> uses_inverse_expr a
    | Expr.Exp a -> uses_inverse_expr a
    | Expr.Log a -> uses_inverse_expr a
    | Expr.Sin a -> uses_inverse_expr a
    | Expr.Cos a -> uses_inverse_expr a
    | Expr.Tan a -> uses_inverse_expr a
    | Expr.Asin a -> uses_inverse_expr a
    | Expr.Acos a -> uses_inverse_expr a
    | Expr.Atan a -> uses_inverse_expr a
    | Expr.Float_of_int a -> uses_inverse_expr a
    | Expr.Int_of_float a -> uses_inverse_expr a
    | Expr.UInt_of_int a -> uses_inverse_expr a
    | Expr.Int_of_uint a -> uses_inverse_expr a
    | Expr.Transpose_m2 a -> uses_inverse_expr a
    | Expr.Transpose_m3 a -> uses_inverse_expr a
    | Expr.Transpose_m4 a | Expr.Determinant_m4 a -> uses_inverse_expr a
    | Expr.Fadd (a, b)
    | Expr.Fsub (a, b)
    | Expr.Fmul (a, b)
    | Expr.Fdiv (a, b)
    | Expr.Flt (a, b)
    | Expr.Fle (a, b)
    | Expr.Fgt (a, b)
    | Expr.Fge (a, b)
    | Expr.Feq_f (a, b)
    | Expr.Fne_f (a, b)
    | Expr.Vec2_of_ff (a, b)
    | Expr.Pow (a, b)
    | Expr.Atan2 (a, b)
    | Expr.Min_f (a, b)
    | Expr.Max_f (a, b)
    | Expr.Step_f (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Iadd (a, b)
    | Expr.Isub (a, b)
    | Expr.Imul (a, b)
    | Expr.Idiv (a, b)
    | Expr.Ilt (a, b)
    | Expr.Ile (a, b)
    | Expr.Igt (a, b)
    | Expr.Ige (a, b)
    | Expr.Ieq (a, b)
    | Expr.Ine (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.And (a, b) | Expr.Or (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Vec3_of_v2_f (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.V2scale (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Dot_v2 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.V2add (a, b) | Expr.V2sub (a, b) | Expr.V2mul (a, b) ->
      uses_inverse_expr a || uses_inverse_expr b
    | Expr.Vec4_of_v3_f (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.V3scale (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Dot_v3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Cross_v3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Distance_v3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Reflect_v3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.V3add (a, b) | Expr.V3sub (a, b) | Expr.V3mul (a, b) ->
      uses_inverse_expr a || uses_inverse_expr b
    | Expr.V4scale (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Dot_v4 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.V4add (a, b) | Expr.V4sub (a, b) | Expr.V4mul (a, b) ->
      uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat2_mul_vec2 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat3_mul_vec3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat4_mul_vec4 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat2_mul_mat2 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat3_mul_mat3 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Mat4_mul_mat4 (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Texture2D (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Texture2DShadow (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.TextureCube (a, b) -> uses_inverse_expr a || uses_inverse_expr b
    | Expr.Vec3_of_fff (a, b, c)
    | Expr.Clamp_f (a, b, c)
    | Expr.Mix_f (a, b, c)
    | Expr.Smoothstep_f (a, b, c) ->
      uses_inverse_expr a || uses_inverse_expr b || uses_inverse_expr c
    | Expr.Vec4_of_v2_ff (a, b, c) ->
      uses_inverse_expr a || uses_inverse_expr b || uses_inverse_expr c
    | Expr.Refract_v3 (a, b, c) ->
      uses_inverse_expr a || uses_inverse_expr b || uses_inverse_expr c
    | Expr.Texture2D_lod (a, b, c) ->
      uses_inverse_expr a || uses_inverse_expr b || uses_inverse_expr c
    | Expr.TexelFetch2D (a, b, c) ->
      uses_inverse_expr a || uses_inverse_expr b || uses_inverse_expr c
    | Expr.Vec4_of_ffff (a, b, c, d) ->
      uses_inverse_expr a
      || uses_inverse_expr b
      || uses_inverse_expr c
      || uses_inverse_expr d
  ;;

  let rec uses_inverse_stmt : type a. a Stmt.stmt -> bool = function
    | Stmt.Assign ((_, _), e) -> uses_inverse_expr e
    | Stmt.Let (typ, _, e) ->
      ignore typ;
      uses_inverse_expr e
    | Stmt.Decl_var (typ, _, e) ->
      ignore typ;
      uses_inverse_expr e
    | Stmt.Expr_stmt e -> uses_inverse_expr e
    | Stmt.Return e -> uses_inverse_expr e
    | Stmt.If (c, thn, els) ->
      uses_inverse_expr c
      || List.exists uses_inverse_stmt thn
      || List.exists uses_inverse_stmt els
    | Stmt.For (_, s, e, body) ->
      uses_inverse_expr s || uses_inverse_expr e || List.exists uses_inverse_stmt body
    | Stmt.While (c, body) | Stmt.DoWhile (body, c) ->
      uses_inverse_expr c || List.exists uses_inverse_stmt body
    | Stmt.Switch (e, cases, dflt) ->
      uses_inverse_expr e
      || List.exists (fun (_, b) -> List.exists uses_inverse_stmt b) cases
      ||
        (match dflt with
        | None -> false
        | Some b -> List.exists uses_inverse_stmt b)
    | Stmt.Block body -> List.exists uses_inverse_stmt body
    | Stmt.Break | Stmt.Continue | Stmt.Discard -> false
  ;;

  let mat4_inverse_helper =
    "fn _mat4_inverse(m: mat4x4<f32>) -> mat4x4<f32> {\n"
    ^ "  let c00 = m[2][2] * m[3][3] - m[3][2] * m[2][3];\n"
    ^ "  let c02 = m[1][2] * m[3][3] - m[3][2] * m[1][3];\n"
    ^ "  let c03 = m[1][2] * m[2][3] - m[2][2] * m[1][3];\n"
    ^ "  let c04 = m[2][1] * m[3][3] - m[3][1] * m[2][3];\n"
    ^ "  let c06 = m[1][1] * m[3][3] - m[3][1] * m[1][3];\n"
    ^ "  let c07 = m[1][1] * m[2][3] - m[2][1] * m[1][3];\n"
    ^ "  let c08 = m[2][1] * m[3][2] - m[3][1] * m[2][2];\n"
    ^ "  let c10 = m[1][1] * m[3][2] - m[3][1] * m[1][2];\n"
    ^ "  let c11 = m[1][1] * m[2][2] - m[2][1] * m[1][2];\n"
    ^ "  let c12 = m[2][0] * m[3][3] - m[3][0] * m[2][3];\n"
    ^ "  let c14 = m[1][0] * m[3][3] - m[3][0] * m[1][3];\n"
    ^ "  let c15 = m[1][0] * m[2][3] - m[2][0] * m[1][3];\n"
    ^ "  let c16 = m[2][0] * m[3][2] - m[3][0] * m[2][2];\n"
    ^ "  let c18 = m[1][0] * m[3][2] - m[3][0] * m[1][2];\n"
    ^ "  let c19 = m[1][0] * m[2][2] - m[2][0] * m[1][2];\n"
    ^ "  let c20 = m[2][0] * m[3][1] - m[3][0] * m[2][1];\n"
    ^ "  let c22 = m[1][0] * m[3][1] - m[3][0] * m[1][1];\n"
    ^ "  let c23 = m[1][0] * m[2][1] - m[2][0] * m[1][1];\n"
    ^ "  let f0 = vec4<f32>(c00, c00, c02, c03);\n"
    ^ "  let f1 = vec4<f32>(c04, c04, c06, c07);\n"
    ^ "  let f2 = vec4<f32>(c08, c08, c10, c11);\n"
    ^ "  let f3 = vec4<f32>(c12, c12, c14, c15);\n"
    ^ "  let f4 = vec4<f32>(c16, c16, c18, c19);\n"
    ^ "  let f5 = vec4<f32>(c20, c20, c22, c23);\n"
    ^ "  let v0 = vec4<f32>(m[1][0], m[0][0], m[0][0], m[0][0]);\n"
    ^ "  let v1 = vec4<f32>(m[1][1], m[0][1], m[0][1], m[0][1]);\n"
    ^ "  let v2 = vec4<f32>(m[1][2], m[0][2], m[0][2], m[0][2]);\n"
    ^ "  let v3 = vec4<f32>(m[1][3], m[0][3], m[0][3], m[0][3]);\n"
    ^ "  let i0 = v1 * f0 - v2 * f1 + v3 * f2;\n"
    ^ "  let i1 = v0 * f0 - v2 * f3 + v3 * f4;\n"
    ^ "  let i2 = v0 * f1 - v1 * f3 + v3 * f5;\n"
    ^ "  let i3 = v0 * f2 - v1 * f4 + v2 * f5;\n"
    ^ "  let sa = vec4<f32>(1.0, -1.0, 1.0, -1.0);\n"
    ^ "  let sb = vec4<f32>(-1.0, 1.0, -1.0, 1.0);\n"
    ^ "  let inv = mat4x4<f32>(i0 * sa, i1 * sb, i2 * sa, i3 * sb);\n"
    ^ "  let r0 = vec4<f32>(inv[0][0], inv[1][0], inv[2][0], inv[3][0]);\n"
    ^ "  let det = dot(m[0], r0);\n"
    ^ "  return inv * (1.0 / det);\n"
    ^ "}\n\n"
  ;;

  let rec expr : type a. ?min_prec:int -> scope -> a Expr.t -> string =
    fun ?(min_prec = p_low) scope e ->
    let go : type b. int -> b Expr.t -> string = fun p x -> expr ~min_prec:p scope x in
    let go0 : type b. b Expr.t -> string = fun x -> expr scope x in
    match e with
    | Expr.Float_const f -> float_lit f
    | Expr.Int_const i -> string_of_int i
    | Expr.UInt_const i -> string_of_int i ^ "u"
    | Expr.Bool_const b -> if b then "true" else "false"
    | Expr.Var (_, name) -> ref_of scope name
    | Expr.Fadd (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.Fsub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.Fmul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.Fdiv (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " / " ^ go (p_mul + 1) b)
    | Expr.Fneg a -> paren ~prec:p_unary ~min_prec ("-" ^ go (p_unary + 1) a)
    | Expr.Iadd (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.Isub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.Imul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.Idiv (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " / " ^ go (p_mul + 1) b)
    | Expr.Ineg a -> paren ~prec:p_unary ~min_prec ("-" ^ go (p_unary + 1) a)
    | Expr.Flt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " < " ^ go (p_rel + 1) b)
    | Expr.Fle (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " <= " ^ go (p_rel + 1) b)
    | Expr.Fgt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " > " ^ go (p_rel + 1) b)
    | Expr.Fge (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " >= " ^ go (p_rel + 1) b)
    | Expr.Feq_f (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " == " ^ go (p_eq + 1) b)
    | Expr.Fne_f (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " != " ^ go (p_eq + 1) b)
    | Expr.Ilt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " < " ^ go (p_rel + 1) b)
    | Expr.Ile (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " <= " ^ go (p_rel + 1) b)
    | Expr.Igt (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " > " ^ go (p_rel + 1) b)
    | Expr.Ige (a, b) ->
      paren ~prec:p_rel ~min_prec (go (p_rel + 1) a ^ " >= " ^ go (p_rel + 1) b)
    | Expr.Ieq (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " == " ^ go (p_eq + 1) b)
    | Expr.Ine (a, b) ->
      paren ~prec:p_eq ~min_prec (go (p_eq + 1) a ^ " != " ^ go (p_eq + 1) b)
    | Expr.And (a, b) ->
      paren ~prec:p_and ~min_prec (go (p_and + 1) a ^ " && " ^ go (p_and + 1) b)
    | Expr.Or (a, b) ->
      paren ~prec:p_or ~min_prec (go (p_or + 1) a ^ " || " ^ go (p_or + 1) b)
    | Expr.Not a -> paren ~prec:p_unary ~min_prec ("!" ^ go (p_unary + 1) a)
    (* vector constructors need explicit generic *)
    | Expr.Vec2_of_f a -> "vec2<f32>(" ^ go0 a ^ ")"
    | Expr.Vec2_of_ff (a, b) -> "vec2<f32>(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Vec3_of_f a -> "vec3<f32>(" ^ go0 a ^ ")"
    | Expr.Vec3_of_fff (a, b, c) ->
      "vec3<f32>(" ^ go0 a ^ ", " ^ go0 b ^ ", " ^ go0 c ^ ")"
    | Expr.Vec3_of_v2_f (v, f) -> "vec3<f32>(" ^ go0 v ^ ", " ^ go0 f ^ ")"
    | Expr.Vec4_of_f a -> "vec4<f32>(" ^ go0 a ^ ")"
    | Expr.Vec4_of_ffff (a, b, c, d) ->
      "vec4<f32>(" ^ go0 a ^ ", " ^ go0 b ^ ", " ^ go0 c ^ ", " ^ go0 d ^ ")"
    | Expr.Vec4_of_v3_f (v, f) -> "vec4<f32>(" ^ go0 v ^ ", " ^ go0 f ^ ")"
    | Expr.Vec4_of_v2_ff (v, f, g) ->
      "vec4<f32>(" ^ go0 v ^ ", " ^ go0 f ^ ", " ^ go0 g ^ ")"
    | Expr.V2add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V2sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V2mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V2scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V3add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V3sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V3mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V3scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V4add (a, b) -> paren ~prec:p_add ~min_prec (go p_add a ^ " + " ^ go p_add b)
    | Expr.V4sub (a, b) ->
      paren ~prec:p_add ~min_prec (go p_add a ^ " - " ^ go (p_add + 1) b)
    | Expr.V4mul (a, b) -> paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go p_mul b)
    | Expr.V4scale (s, v) -> paren ~prec:p_mul ~min_prec (go p_mul s ^ " * " ^ go p_mul v)
    | Expr.V2x v -> go p_post v ^ ".x"
    | Expr.V2y v -> go p_post v ^ ".y"
    | Expr.V3x v -> go p_post v ^ ".x"
    | Expr.V3y v -> go p_post v ^ ".y"
    | Expr.V3z v -> go p_post v ^ ".z"
    | Expr.V4x v -> go p_post v ^ ".x"
    | Expr.V4y v -> go p_post v ^ ".y"
    | Expr.V4z v -> go p_post v ^ ".z"
    | Expr.V4w v -> go p_post v ^ ".w"
    | Expr.V4xyz v -> go p_post v ^ ".xyz"
    | Expr.V4xy v -> go p_post v ^ ".xy"
    | Expr.Dot_v2 (a, b) -> "dot(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Dot_v3 (a, b) -> "dot(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Dot_v4 (a, b) -> "dot(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Cross_v3 (a, b) -> "cross(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Length_v2 v -> "length(" ^ go0 v ^ ")"
    | Expr.Length_v3 v -> "length(" ^ go0 v ^ ")"
    | Expr.Length_v4 v -> "length(" ^ go0 v ^ ")"
    | Expr.Distance_v3 (a, b) -> "distance(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Normalize_v2 v -> "normalize(" ^ go0 v ^ ")"
    | Expr.Normalize_v3 v -> "normalize(" ^ go0 v ^ ")"
    | Expr.Normalize_v4 v -> "normalize(" ^ go0 v ^ ")"
    | Expr.Reflect_v3 (i, n) -> "reflect(" ^ go0 i ^ ", " ^ go0 n ^ ")"
    | Expr.Refract_v3 (i, n, e) -> "refract(" ^ go0 i ^ ", " ^ go0 n ^ ", " ^ go0 e ^ ")"
    | Expr.Mat2_mul_vec2 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat3_mul_vec3 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat4_mul_vec4 (m, v) ->
      paren ~prec:p_mul ~min_prec (go p_mul m ^ " * " ^ go (p_mul + 1) v)
    | Expr.Mat2_mul_mat2 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Mat3_mul_mat3 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Mat4_mul_mat4 (a, b) ->
      paren ~prec:p_mul ~min_prec (go p_mul a ^ " * " ^ go (p_mul + 1) b)
    | Expr.Transpose_m2 m -> "transpose(" ^ go0 m ^ ")"
    | Expr.Transpose_m3 m -> "transpose(" ^ go0 m ^ ")"
    | Expr.Transpose_m4 m -> "transpose(" ^ go0 m ^ ")"
    | Expr.Inverse_m4 m -> "_mat4_inverse(" ^ go0 m ^ ")"
    | Expr.Determinant_m4 m -> "determinant(" ^ go0 m ^ ")"
    | Expr.Abs_f a -> "abs(" ^ go0 a ^ ")"
    | Expr.Sign_f a -> "sign(" ^ go0 a ^ ")"
    | Expr.Floor a -> "floor(" ^ go0 a ^ ")"
    | Expr.Ceil a -> "ceil(" ^ go0 a ^ ")"
    | Expr.Fract a -> "fract(" ^ go0 a ^ ")"
    | Expr.Sqrt a -> "sqrt(" ^ go0 a ^ ")"
    | Expr.InverseSqrt a -> "inverseSqrt(" ^ go0 a ^ ")"
    | Expr.Pow (a, b) -> "pow(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Exp a -> "exp(" ^ go0 a ^ ")"
    | Expr.Log a -> "log(" ^ go0 a ^ ")"
    | Expr.Sin a -> "sin(" ^ go0 a ^ ")"
    | Expr.Cos a -> "cos(" ^ go0 a ^ ")"
    | Expr.Tan a -> "tan(" ^ go0 a ^ ")"
    | Expr.Asin a -> "asin(" ^ go0 a ^ ")"
    | Expr.Acos a -> "acos(" ^ go0 a ^ ")"
    | Expr.Atan a -> "atan(" ^ go0 a ^ ")"
    | Expr.Atan2 (y, x) -> "atan2(" ^ go0 y ^ ", " ^ go0 x ^ ")"
    | Expr.Min_f (a, b) -> "min(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Max_f (a, b) -> "max(" ^ go0 a ^ ", " ^ go0 b ^ ")"
    | Expr.Clamp_f (x, lo, hi) -> "clamp(" ^ go0 x ^ ", " ^ go0 lo ^ ", " ^ go0 hi ^ ")"
    | Expr.Mix_f (a, b, t) -> "mix(" ^ go0 a ^ ", " ^ go0 b ^ ", " ^ go0 t ^ ")"
    | Expr.Step_f (e, x) -> "step(" ^ go0 e ^ ", " ^ go0 x ^ ")"
    | Expr.Smoothstep_f (e0, e1, x) ->
      "smoothstep(" ^ go0 e0 ^ ", " ^ go0 e1 ^ ", " ^ go0 x ^ ")"
    | Expr.Float_of_int i -> "f32(" ^ go0 i ^ ")"
    | Expr.Int_of_float f -> "i32(" ^ go0 f ^ ")"
    | Expr.UInt_of_int i -> "u32(" ^ go0 i ^ ")"
    | Expr.Int_of_uint u -> "i32(" ^ go0 u ^ ")"
    | Expr.Texture2D (s, uv) ->
      "textureSample(" ^ go0 s ^ ", " ^ sampler_name s ^ ", " ^ go0 uv ^ ")"
    | Expr.Texture2D_lod (s, uv, l) ->
      "textureSampleLevel("
      ^ go0 s
      ^ ", "
      ^ sampler_name s
      ^ ", "
      ^ go0 uv
      ^ ", "
      ^ go0 l
      ^ ")"
    | Expr.Texture2DShadow (s, uv) ->
      let uv_s = go p_post uv in
      "textureSampleCompare("
      ^ go0 s
      ^ ", "
      ^ sampler_name s
      ^ ", "
      ^ uv_s
      ^ ".xy, "
      ^ uv_s
      ^ ".z)"
    | Expr.TextureCube (s, d) ->
      "textureSample(" ^ go0 s ^ ", " ^ sampler_name s ^ ", " ^ go0 d ^ ")"
    | Expr.TexelFetch2D (s, c, l) ->
      "textureLoad(" ^ go0 s ^ ", " ^ go0 c ^ ", " ^ go0 l ^ ")"
  ;;

  let rec stmt : type a. indent:int -> scope -> a Stmt.stmt -> string =
    fun ~indent scope s ->
    let p1 = pad indent
    and p2 = pad (indent + 2) in
    let seq n ss = String.concat "" (List.map (stmt ~indent:n scope) ss) in
    match s with
    | Stmt.Assign ((_, name), e) -> p1 ^ ref_of scope name ^ " = " ^ expr scope e ^ ";\n"
    | Stmt.Let (typ, name, e) ->
      p1 ^ "let " ^ name ^ ": " ^ Types.wgsl_name typ ^ " = " ^ expr scope e ^ ";\n"
    | Stmt.Decl_var (typ, name, e) ->
      p1 ^ "var " ^ name ^ ": " ^ Types.wgsl_name typ ^ " = " ^ expr scope e ^ ";\n"
    | Stmt.Expr_stmt e -> p1 ^ expr scope e ^ ";\n"
    | Stmt.If (c, thn, els) ->
      if els = []
      then p1 ^ "if " ^ expr scope c ^ " {\n" ^ seq (indent + 2) thn ^ p1 ^ "}\n"
      else
        p1
        ^ "if "
        ^ expr scope c
        ^ " {\n"
        ^ seq (indent + 2) thn
        ^ p1
        ^ "} else {\n"
        ^ seq (indent + 2) els
        ^ p1
        ^ "}\n"
    | Stmt.For ((typ, i), start, stop, body) ->
      p1
      ^ "for (var "
      ^ i
      ^ ": "
      ^ Types.wgsl_name typ
      ^ " = "
      ^ expr scope start
      ^ "; "
      ^ i
      ^ " < "
      ^ expr scope stop
      ^ "; "
      ^ i
      ^ "++) {\n"
      ^ seq (indent + 2) body
      ^ p1
      ^ "}\n"
    | Stmt.While (c, body) ->
      p1 ^ "while " ^ expr scope c ^ " {\n" ^ seq (indent + 2) body ^ p1 ^ "}\n"
    | Stmt.DoWhile (body, c) ->
      p1
      ^ "loop {\n"
      ^ seq (indent + 2) body
      ^ p2
      ^ "if !("
      ^ expr scope c
      ^ ") { break; }\n"
      ^ p1
      ^ "}\n"
    | Stmt.Switch (e, cases, default) ->
      let case_s (v, body) =
        p2 ^ "case " ^ string_of_int v ^ ": {\n" ^ seq (indent + 4) body ^ p2 ^ "}\n"
      in
      let dflt =
        match default with
        | None -> p2 ^ "default: {}\n"
        | Some body -> p2 ^ "default: {\n" ^ seq (indent + 4) body ^ p2 ^ "}\n"
      in
      p1
      ^ "switch "
      ^ expr scope e
      ^ " {\n"
      ^ String.concat "" (List.map case_s cases)
      ^ dflt
      ^ p1
      ^ "}\n"
    | Stmt.Break -> p1 ^ "break;\n"
    | Stmt.Continue -> p1 ^ "continue;\n"
    | Stmt.Discard -> p1 ^ "discard;\n"
    | Stmt.Block body -> p1 ^ "{\n" ^ seq (indent + 2) body ^ p1 ^ "}\n"
    | Stmt.Return e -> p1 ^ "return " ^ expr scope e ^ ";\n"
  ;;

  (* ----- helpers to emit struct and resource definitions ----- *)
  let emit_location_struct name fields =
    let b = Buffer.create 256 in
    Buffer.add_string b (Printf.sprintf "struct %s {\n" name);
    List.iteri
      (fun i (Any typ, n) ->
         Buffer.add_string
           b
           (Printf.sprintf "  @location(%d) %s: %s,\n" i n (Types.wgsl_name typ)))
      fields;
    Buffer.add_string b "}\n\n";
    Buffer.contents b
  ;;

  let emit_vertex_output_struct varys_no_pos =
    let b = Buffer.create 512 in
    Buffer.add_string b "struct VertexOutput {\n";
    Buffer.add_string b "  @builtin(position) position: vec4<f32>,\n";
    List.iteri
      (fun i (Any typ, n) ->
         Buffer.add_string
           b
           (Printf.sprintf "  @location(%d) %s: %s,\n" i n (Types.wgsl_name typ)))
      varys_no_pos;
    Buffer.add_string b "}\n\n";
    Buffer.contents b
  ;;

  let emit_uniforms b unifs =
    let data_unifs, tex_unifs = List.partition (fun (t, _) -> not (is_sampler t)) unifs in
    let binding = ref 0 in
    if data_unifs <> []
    then (
      Buffer.add_string b "struct Uniforms {\n";
      List.iter
        (fun (Any typ, n) ->
           Buffer.add_string b (Printf.sprintf "  %s: %s,\n" n (Types.wgsl_name typ)))
        data_unifs;
      Buffer.add_string b "}\n\n";
      Buffer.add_string
        b
        (Printf.sprintf
           "@group(0) @binding(%d) var<uniform> uniforms: Uniforms;\n"
           !binding);
      incr binding);
    List.iter
      (fun ((Any typ as at), n) ->
         Buffer.add_string
           b
           (Printf.sprintf
              "@group(0) @binding(%d) var %s: %s;\n"
              !binding
              n
              (Types.wgsl_name typ));
         incr binding;
         match sampler_kind at with
         | Some sk ->
           Buffer.add_string
             b
             (Printf.sprintf "@group(0) @binding(%d) var %s_sampler: %s;\n" !binding n sk);
           incr binding
         | None -> ())
      tex_unifs;
    if data_unifs <> [] || tex_unifs <> [] then Buffer.add_string b "\n";
    List.map snd data_unifs
  ;;

  (* ----- vertex shader ----- *)
  let emit_vertex (sh : (Shader.vertex_stage, _, _, _) Shader.t) : string =
    let b = Buffer.create 4096 in
    let addf fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    let varys_no_pos = filter_varys sh.varys in
    if sh.attrs <> []
    then Buffer.add_string b (emit_location_struct "VertexInput" sh.attrs);
    Buffer.add_string b (emit_vertex_output_struct varys_no_pos);
    let data_unif_names = emit_uniforms b sh.unifs in
    if List.exists uses_inverse_stmt sh.body then Buffer.add_string b mat4_inverse_helper;
    if sh.attrs <> []
    then addf "@vertex\nfn main(input: VertexInput) -> VertexOutput {\n"
    else addf "@vertex\nfn main() -> VertexOutput {\n";
    addf "  var output: VertexOutput;\n";
    let scope =
      { attrs = List.map snd sh.attrs
      ; unifs = data_unif_names
      ; varys = List.map snd varys_no_pos
      ; outs = []
      ; is_vertex = true
      }
    in
    List.iter (fun s -> Buffer.add_string b (stmt ~indent:2 scope s)) sh.body;
    addf "  return output;\n}\n";
    Buffer.contents b
  ;;

  (* ----- fragment shader ----- *)
  let emit_fragment (sh : (Shader.fragment_stage, _, _, _) Shader.t) : string =
    let b = Buffer.create 4096 in
    let addf fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    let varys_no_pos = filter_varys sh.varys in
    if varys_no_pos <> []
    then Buffer.add_string b (emit_location_struct "FragmentInput" varys_no_pos);
    if sh.outs <> []
    then Buffer.add_string b (emit_location_struct "FragmentOutput" sh.outs);
    let data_unif_names = emit_uniforms b sh.unifs in
    if List.exists uses_inverse_stmt sh.body then Buffer.add_string b mat4_inverse_helper;
    let params = if varys_no_pos <> [] then "input: FragmentInput" else "" in
    let ret = if sh.outs <> [] then " -> FragmentOutput" else "" in
    addf "@fragment\nfn main(%s)%s {\n" params ret;
    if sh.outs <> [] then addf "  var output: FragmentOutput;\n";
    let scope =
      { attrs = []
      ; unifs = data_unif_names
      ; varys = List.map snd varys_no_pos
      ; outs = List.map snd sh.outs
      ; is_vertex = false
      }
    in
    List.iter (fun s -> Buffer.add_string b (stmt ~indent:2 scope s)) sh.body;
    if sh.outs <> [] then addf "  return output;\n";
    addf "}\n";
    Buffer.contents b
  ;;
end

(* ================================================================== *)
(* Interface validation & Public API                                   *)
(* ================================================================== *)

let check_interface
      (v : (Shader.vertex_stage, _, _, _) Shader.t)
      (f : (Shader.fragment_stage, _, _, _) Shader.t)
  : (unit, string) result
  =
  let v_varys = filter_varys v.varys in
  let f_varys = filter_varys f.varys in
  let rec go i vs fs =
    match vs, fs with
    | [], [] -> Ok ()
    | (vt, vn) :: vrest, (ft, fn) :: frest ->
      if vn <> fn
      then
        Error
          (Printf.sprintf
             "varying mismatch at location %d: vertex outputs %S, fragment expects %S"
             i
             vn
             fn)
      else if not (Shader.equal_any_typ vt ft)
      then
        Error
          (Printf.sprintf
             "varying %S type mismatch: vertex outputs %s, fragment expects %s"
             vn
             (Shader.string_of_any_typ vt)
             (Shader.string_of_any_typ ft))
      else go (i + 1) vrest frest
    | (_, vn) :: _, [] ->
      Error
        (Printf.sprintf
           "vertex shader outputs extra varying %S not consumed by fragment shader"
           vn)
    | [], (_, fn) :: _ ->
      Error
        (Printf.sprintf
           "fragment shader expects varying %S not provided by vertex shader"
           fn)
  in
  go 0 v_varys f_varys
;;

let compile_vertex ?(backend = GLSL) (sh : (Shader.vertex_stage, _, _, _) Shader.t)
  : string
  =
  match backend with
  | GLSL -> Glsl.emit_vertex sh
  | WGSL -> Wgsl.emit_vertex sh
;;

let compile_fragment ?(backend = GLSL) (sh : (Shader.fragment_stage, _, _, _) Shader.t)
  : string
  =
  match backend with
  | GLSL -> Glsl.emit_fragment sh
  | WGSL -> Wgsl.emit_fragment sh
;;

let compile_pair
      (backend : backend)
      (v : (Shader.vertex_stage, _, _, _) Shader.t)
      (f : (Shader.fragment_stage, _, _, _) Shader.t)
  : (string * string, string) result
  =
  match check_interface v f with
  | Error _ as err -> err
  | Ok () -> Ok (compile_vertex ~backend v, compile_fragment ~backend f)
;;
