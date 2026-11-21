
(* ------------------------------------------------------------------ *)
(* Phantom types — one per concrete GLSL/WGSL type                    *)
(* ------------------------------------------------------------------ *)

type float_s
type int_s
type uint_s
type bool_s

type vec2
type vec3
type vec4
type ivec2
type ivec3
type ivec4
type uvec2
type uvec3
type uvec4
type bvec2
type bvec3
type bvec4

type mat2
type mat3
type mat4

type sampler2d
type sampler3d
type sampler_cube
type sampler2d_array
type sampler2d_shadow
type isampler2d
type usampler2d

(* ------------------------------------------------------------------ *)
(* Type GADT                                                          *)
(* ------------------------------------------------------------------ *)

type _ t =
  (* scalars *)
  | Float  : float_s t
  | Int    : int_s t
  | UInt   : uint_s t
  | Bool   : bool_s t
  (* float vectors *)
  | Vec2   : vec2 t
  | Vec3   : vec3 t
  | Vec4   : vec4 t
  (* int vectors *)
  | IVec2  : ivec2 t
  | IVec3  : ivec3 t
  | IVec4  : ivec4 t
  (* uint vectors *)
  | UVec2  : uvec2 t
  | UVec3  : uvec3 t
  | UVec4  : uvec4 t
  (* bool vectors *)
  | BVec2  : bvec2 t
  | BVec3  : bvec3 t
  | BVec4  : bvec4 t
  (* matrices *)
  | Mat2   : mat2 t
  | Mat3   : mat3 t
  | Mat4   : mat4 t
  (* samplers *)
  | Sampler2D       : sampler2d t
  | Sampler3D       : sampler3d t
  | SamplerCube     : sampler_cube t
  | Sampler2DArray  : sampler2d_array t
  | Sampler2DShadow : sampler2d_shadow t
  | ISampler2D      : isampler2d t
  | USampler2D      : usampler2d t

(* ------------------------------------------------------------------ *)
(* Codegen: GLSL name. WGSL override handled by the transpiler.       *)
(* ------------------------------------------------------------------ *)

let glsl_name : type a. a t -> string = function
  | Float -> "float" | Int -> "int" | UInt -> "uint" | Bool -> "bool"
  | Vec2 -> "vec2" | Vec3 -> "vec3" | Vec4 -> "vec4"
  | IVec2 -> "ivec2" | IVec3 -> "ivec3" | IVec4 -> "ivec4"
  | UVec2 -> "uvec2" | UVec3 -> "uvec3" | UVec4 -> "uvec4"
  | BVec2 -> "bvec2" | BVec3 -> "bvec3" | BVec4 -> "bvec4"
  | Mat2 -> "mat2" | Mat3 -> "mat3" | Mat4 -> "mat4"
  | Sampler2D -> "sampler2D" | Sampler3D -> "sampler3D"
  | SamplerCube -> "samplerCube" | Sampler2DArray -> "sampler2DArray"
  | Sampler2DShadow -> "sampler2DShadow"
  | ISampler2D -> "isampler2D" | USampler2D -> "usampler2D"

(* WGSL name — for the same type, WGSL syntax differs *)
let wgsl_name : type a. a t -> string = function
  | Float -> "f32" | Int -> "i32" | UInt -> "u32" | Bool -> "bool"
  | Vec2 -> "vec2<f32>" | Vec3 -> "vec3<f32>" | Vec4 -> "vec4<f32>"
  | IVec2 -> "vec2<i32>" | IVec3 -> "vec3<i32>" | IVec4 -> "vec4<i32>"
  | UVec2 -> "vec2<u32>" | UVec3 -> "vec3<u32>" | UVec4 -> "vec4<u32>"
  | BVec2 -> "vec2<bool>" | BVec3 -> "vec3<bool>" | BVec4 -> "vec4<bool>"
  | Mat2 -> "mat2x2<f32>" | Mat3 -> "mat3x3<f32>" | Mat4 -> "mat4x4<f32>"
  | Sampler2D -> "texture_2d<f32>" | Sampler3D -> "texture_3d<f32>"
  | SamplerCube -> "texture_cube<f32>" | Sampler2DArray -> "texture_2d_array<f32>"
  | Sampler2DShadow -> "texture_depth_2d"
  | ISampler2D -> "texture_2d<i32>" | USampler2D -> "texture_2d<u32>"
