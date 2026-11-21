
open Types

type _ t =
  (* ---------------- Constants ---------------- *)
  | Float_const : float -> float_s t
  | Int_const   : int   -> int_s t
  | UInt_const  : int   -> uint_s t
  | Bool_const  : bool  -> bool_s t

  (* ---------------- Variables ---------------- *)
  | Var : 'a Types.t * string -> 'a t

  (* ---------------- Float scalar arithmetic ---------------- *)
  | Fadd : float_s t * float_s t -> float_s t
  | Fsub : float_s t * float_s t -> float_s t
  | Fmul : float_s t * float_s t -> float_s t
  | Fdiv : float_s t * float_s t -> float_s t
  | Fneg : float_s t -> float_s t

  (* ---------------- Int scalar arithmetic ---------------- *)
  | Iadd : int_s t * int_s t -> int_s t
  | Isub : int_s t * int_s t -> int_s t
  | Imul : int_s t * int_s t -> int_s t
  | Idiv : int_s t * int_s t -> int_s t
  | Ineg : int_s t -> int_s t

  (* ---------------- Scalar comparisons ---------------- *)
  | Flt : float_s t * float_s t -> bool_s t
  | Fle : float_s t * float_s t -> bool_s t
  | Fgt : float_s t * float_s t -> bool_s t
  | Fge : float_s t * float_s t -> bool_s t
  | Feq_f : float_s t * float_s t -> bool_s t
  | Fne_f : float_s t * float_s t -> bool_s t
  | Ilt : int_s t * int_s t -> bool_s t
  | Ile : int_s t * int_s t -> bool_s t
  | Igt : int_s t * int_s t -> bool_s t
  | Ige : int_s t * int_s t -> bool_s t
  | Ieq : int_s t * int_s t -> bool_s t
  | Ine : int_s t * int_s t -> bool_s t

  (* ---------------- Bool logic ---------------- *)
  | And : bool_s t * bool_s t -> bool_s t
  | Or  : bool_s t * bool_s t -> bool_s t
  | Not : bool_s t -> bool_s t

  (* ---------------- Vector construction ---------------- *)
  | Vec2_of_f    : float_s t -> vec2 t
  | Vec2_of_ff   : float_s t * float_s t -> vec2 t
  | Vec3_of_f    : float_s t -> vec3 t
  | Vec3_of_fff  : float_s t * float_s t * float_s t -> vec3 t
  | Vec3_of_v2_f : vec2 t * float_s t -> vec3 t
  | Vec4_of_f    : float_s t -> vec4 t
  | Vec4_of_ffff : float_s t * float_s t * float_s t * float_s t -> vec4 t
  | Vec4_of_v3_f : vec3 t * float_s t -> vec4 t
  | Vec4_of_v2_ff: vec2 t * float_s t * float_s t -> vec4 t

  (* ---------------- Vector arithmetic (float vecs) ---------------- *)
  | V2add   : vec2 t * vec2 t -> vec2 t
  | V2sub   : vec2 t * vec2 t -> vec2 t
  | V2mul   : vec2 t * vec2 t -> vec2 t
  | V2scale : float_s t * vec2 t -> vec2 t
  | V3add   : vec3 t * vec3 t -> vec3 t
  | V3sub   : vec3 t * vec3 t -> vec3 t
  | V3mul   : vec3 t * vec3 t -> vec3 t
  | V3scale : float_s t * vec3 t -> vec3 t
  | V4add   : vec4 t * vec4 t -> vec4 t
  | V4sub   : vec4 t * vec4 t -> vec4 t
  | V4mul   : vec4 t * vec4 t -> vec4 t
  | V4scale : float_s t * vec4 t -> vec4 t

  (* ---------------- Swizzles (component selection) ---------------- *)
  | V2x : vec2 t -> float_s t
  | V2y : vec2 t -> float_s t
  | V3x : vec3 t -> float_s t
  | V3y : vec3 t -> float_s t
  | V3z : vec3 t -> float_s t
  | V4x : vec4 t -> float_s t
  | V4y : vec4 t -> float_s t
  | V4z : vec4 t -> float_s t
  | V4w : vec4 t -> float_s t
  | V4xyz : vec4 t -> vec3 t
  | V4xy  : vec4 t -> vec2 t

  (* ---------------- Geometric builtins ---------------- *)
  | Dot_v2    : vec2 t * vec2 t -> float_s t
  | Dot_v3    : vec3 t * vec3 t -> float_s t
  | Dot_v4    : vec4 t * vec4 t -> float_s t
  | Cross_v3  : vec3 t * vec3 t -> vec3 t
  | Length_v2 : vec2 t -> float_s t
  | Length_v3 : vec3 t -> float_s t
  | Length_v4 : vec4 t -> float_s t
  | Distance_v3 : vec3 t * vec3 t -> float_s t
  | Normalize_v2 : vec2 t -> vec2 t
  | Normalize_v3 : vec3 t -> vec3 t
  | Normalize_v4 : vec4 t -> vec4 t
  | Reflect_v3   : vec3 t * vec3 t -> vec3 t
  | Refract_v3   : vec3 t * vec3 t * float_s t -> vec3 t

  (* ---------------- Matrix ops ---------------- *)
  | Mat2_mul_vec2 : mat2 t * vec2 t -> vec2 t
  | Mat3_mul_vec3 : mat3 t * vec3 t -> vec3 t
  | Mat4_mul_vec4 : mat4 t * vec4 t -> vec4 t
  | Mat2_mul_mat2 : mat2 t * mat2 t -> mat2 t
  | Mat3_mul_mat3 : mat3 t * mat3 t -> mat3 t
  | Mat4_mul_mat4 : mat4 t * mat4 t -> mat4 t
  | Transpose_m2 : mat2 t -> mat2 t
  | Transpose_m3 : mat3 t -> mat3 t
  | Transpose_m4 : mat4 t -> mat4 t
  | Inverse_m4   : mat4 t -> mat4 t
  | Determinant_m4 : mat4 t -> float_s t

  (* ---------------- Math builtins ---------------- *)
  | Abs_f          : float_s t -> float_s t
  | Sign_f         : float_s t -> float_s t
  | Floor          : float_s t -> float_s t
  | Ceil           : float_s t -> float_s t
  | Fract          : float_s t -> float_s t
  | Sqrt           : float_s t -> float_s t
  | InverseSqrt    : float_s t -> float_s t
  | Pow            : float_s t * float_s t -> float_s t
  | Exp            : float_s t -> float_s t
  | Log            : float_s t -> float_s t
  | Sin            : float_s t -> float_s t
  | Cos            : float_s t -> float_s t
  | Tan            : float_s t -> float_s t
  | Asin           : float_s t -> float_s t
  | Acos           : float_s t -> float_s t
  | Atan           : float_s t -> float_s t
  | Atan2          : float_s t * float_s t -> float_s t
  | Min_f          : float_s t * float_s t -> float_s t
  | Max_f          : float_s t * float_s t -> float_s t
  | Clamp_f        : float_s t * float_s t * float_s t -> float_s t
  | Mix_f          : float_s t * float_s t * float_s t -> float_s t
  | Step_f         : float_s t * float_s t -> float_s t
  | Smoothstep_f   : float_s t * float_s t * float_s t -> float_s t

  (* ---------------- Conversions ---------------- *)
  | Float_of_int   : int_s t -> float_s t
  | Int_of_float   : float_s t -> int_s t
  | UInt_of_int    : int_s t -> uint_s t
  | Int_of_uint    : uint_s t -> int_s t

  (* ---------------- Texture sampling ---------------- *)
  | Texture2D      : sampler2d t * vec2 t -> vec4 t
  | Texture2D_lod  : sampler2d t * vec2 t * float_s t -> vec4 t
  | Texture2DShadow: sampler2d_shadow t * vec3 t -> float_s t
  | TextureCube    : sampler_cube t * vec3 t -> vec4 t
  | TexelFetch2D   : sampler2d t * ivec2 t * int_s t -> vec4 t
