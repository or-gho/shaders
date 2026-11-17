type _ ty =
  | Float : float ty
  | Int   : int ty
  | Bool  : bool ty
  | Vec2  : vec2 ty
  | Vec3  : vec3 ty
  | Vec4  : vec4 ty
  | Mat2  : mat2 ty
  | Mat3  : mat3 ty
  | Mat4  : mat4 ty
  | Sampler2D : sampler2d ty

and vec2
and vec3
and vec4
and mat2
and mat3
and mat4
and sampler2d
