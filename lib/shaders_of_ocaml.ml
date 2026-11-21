open Shaders_utils

(* ---- Phantom markers, equal to the internal ones ---- *)
type float_s = Types.float_s
type int_s = Types.int_s
type uint_s = Types.uint_s
type bool_s = Types.bool_s
type vec2 = Types.vec2
type vec3 = Types.vec3
type vec4 = Types.vec4
type ivec2 = Types.ivec2
type ivec3 = Types.ivec3
type ivec4 = Types.ivec4
type uvec2 = Types.uvec2
type uvec3 = Types.uvec3
type uvec4 = Types.uvec4
type bvec2 = Types.bvec2
type bvec3 = Types.bvec3
type bvec4 = Types.bvec4
type mat2 = Types.mat2
type mat3 = Types.mat3
type mat4 = Types.mat4
type sampler2d = Types.sampler2d
type sampler3d = Types.sampler3d
type sampler_cube = Types.sampler_cube
type sampler2d_shadow = Types.sampler2d_shadow

(* ---- Core types ---- *)
type 'a ty = 'a Types.t
type 'a expr = 'a Expr.t
type 'a var = 'a Stmt.ref_
type stmt = unit Stmt.stmt
type block = stmt list

type backend = Transpile.backend =
  | GLSL
  | WGSL

type program =
  { vertex : string
  ; fragment : string
  }

type error = Interface_mismatch of string

let string_of_error (Interface_mismatch msg) = "shader interface mismatch: " ^ msg

type vertex_stage = Shader.vertex_stage
type fragment_stage = Shader.fragment_stage

type ('stage, 'inputs, 'varyings, 'outputs) builder =
  ('stage, 'inputs, 'varyings, 'outputs) Shader.builder

type ('stage, 'inputs, 'varyings, 'outputs) shader =
  ('stage, 'inputs, 'varyings, 'outputs) Shader.shader

(* ---- Type descriptors ---- *)
module Ty = struct
  let float = Types.Float
  let int = Types.Int
  let uint = Types.UInt
  let bool = Types.Bool
  let vec2 = Types.Vec2
  let vec3 = Types.Vec3
  let vec4 = Types.Vec4
  let ivec2 = Types.IVec2
  let ivec3 = Types.IVec3
  let ivec4 = Types.IVec4
  let uvec2 = Types.UVec2
  let uvec3 = Types.UVec3
  let uvec4 = Types.UVec4
  let bvec2 = Types.BVec2
  let bvec3 = Types.BVec3
  let bvec4 = Types.BVec4
  let mat2 = Types.Mat2
  let mat3 = Types.Mat3
  let mat4 = Types.Mat4
  let sampler2d = Types.Sampler2D
  let sampler3d = Types.Sampler3D
  let sampler_cube = Types.SamplerCube
  let sampler2d_shadow = Types.Sampler2DShadow
end

(* ---- Expressions ---- *)
module E = struct
  let float x = Expr.Float_const x
  let int x = Expr.Int_const x
  let uint x = Expr.UInt_const x
  let bool x = Expr.Bool_const x
  let add_f a b = Expr.Fadd (a, b)
  let sub_f a b = Expr.Fsub (a, b)
  let mul_f a b = Expr.Fmul (a, b)
  let div_f a b = Expr.Fdiv (a, b)
  let neg_f a = Expr.Fneg a
  let add_i a b = Expr.Iadd (a, b)
  let sub_i a b = Expr.Isub (a, b)
  let mul_i a b = Expr.Imul (a, b)
  let div_i a b = Expr.Idiv (a, b)
  let lt_f a b = Expr.Flt (a, b)
  let le_f a b = Expr.Fle (a, b)
  let gt_f a b = Expr.Fgt (a, b)
  let ge_f a b = Expr.Fge (a, b)
  let lt_i a b = Expr.Ilt (a, b)
  let eq_i a b = Expr.Ieq (a, b)
  let and_ a b = Expr.And (a, b)
  let or_ a b = Expr.Or (a, b)
  let not_ a = Expr.Not a
  let vec2 x y = Expr.Vec2_of_ff (x, y)
  let vec3 x y z = Expr.Vec3_of_fff (x, y, z)
  let vec4 x y z w = Expr.Vec4_of_ffff (x, y, z, w)
  let vec4_of_vec3 v w = Expr.Vec4_of_v3_f (v, w)
  let add_v2 a b = Expr.V2add (a, b)
  let scale_v2 s v = Expr.V2scale (s, v)
  let add_v3 a b = Expr.V3add (a, b)
  let sub_v3 a b = Expr.V3sub (a, b)
  let scale_v3 s v = Expr.V3scale (s, v)
  let add_v4 a b = Expr.V4add (a, b)
  let scale_v4 s v = Expr.V4scale (s, v)
  let v2x v = Expr.V2x v
  let v2y v = Expr.V2y v
  let v3x v = Expr.V3x v
  let v3y v = Expr.V3y v
  let v3z v = Expr.V3z v
  let v4x v = Expr.V4x v
  let v4y v = Expr.V4y v
  let v4z v = Expr.V4z v
  let v4w v = Expr.V4w v
  let v4xy v = Expr.V4xy v
  let v4xyz v = Expr.V4xyz v
  let dot3 a b = Expr.Dot_v3 (a, b)
  let cross3 a b = Expr.Cross_v3 (a, b)
  let length3 v = Expr.Length_v3 v
  let normalize3 v = Expr.Normalize_v3 v
  let distance3 a b = Expr.Distance_v3 (a, b)
  let reflect3 a b = Expr.Reflect_v3 (a, b)
  let mat4_mul_vec4 m v = Expr.Mat4_mul_vec4 (m, v)
  let abs_f x = Expr.Abs_f x
  let floor_f x = Expr.Floor x
  let fract_f x = Expr.Fract x
  let sqrt x = Expr.Sqrt x
  let pow a b = Expr.Pow (a, b)
  let sin x = Expr.Sin x
  let cos x = Expr.Cos x
  let min_f a b = Expr.Min_f (a, b)
  let max_f a b = Expr.Max_f (a, b)
  let clamp_f x lo hi = Expr.Clamp_f (x, lo, hi)
  let mix_f a b t = Expr.Mix_f (a, b, t)
  let step_f edge x = Expr.Step_f (edge, x)
  let smoothstep_f lo hi x = Expr.Smoothstep_f (lo, hi, x)
  let float_of_int x = Expr.Float_of_int x
  let int_of_float x = Expr.Int_of_float x
  let texture2d s uv = Expr.Texture2D (s, uv)
  let texture2d_lod s uv lod = Expr.Texture2D_lod (s, uv, lod)
  let texture_cube s dir = Expr.TextureCube (s, dir)
end

(* ---- Statements ---- *)
module S = struct
  let counter = ref 0

  let fresh () =
    incr counter;
    Printf.sprintf "_l%d" !counter
  ;;

  let let_ ty init k =
    let name = fresh () in
    Stmt.Let (ty, name, init) :: k (ty, name)
  ;;

  let read ((ty, name) : 'a var) : 'a expr = Expr.Var (ty, name)
  let assign v e = Stmt.Assign (v, e)
  let if_ c ~then_ ~else_ = Stmt.If (c, then_, else_)
  let while_ c body = Stmt.While (c, body)
  let for_ v start stop body = Stmt.For (v, start, stop, body)
  let break_ = Stmt.Break
  let continue_ = Stmt.Continue
  let discard = Stmt.Discard
  let eval e = Stmt.Expr_stmt e
end

(* ---- Stages ---- *)
module Vertex = struct
  let create = Shader.create_vertex
  let attribute = Shader.with_attribute
  let uniform = Shader.with_uniform
  let varying = Shader.with_varying
  let body = Shader.vertex_body
end

module Fragment = struct
  let create = Shader.create_fragment
  let uniform = Shader.with_uniform
  let varying = Shader.with_varying
  let output = Shader.with_output
  let body = Shader.fragment_body
end

(* ---- Compilation ---- *)
module Compile = struct
  let pair ?(backend = GLSL) vs fs =
    match Transpile.compile_pair backend vs fs with
    | Ok (vertex, fragment) -> Ok { vertex; fragment }
    | Error msg -> Error (Interface_mismatch msg)
  ;;
end
