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
  type sampler2d_shadow

  (** {1 Core types} *)

  type 'a ty
  (** Runtime descriptor of a shader type. *)

  type 'a expr
  (** An expression of shader type ['a]. *)

  type 'a var
  (** A declared local variable of shader type ['a]. Reads and writes go
      through {!S.read} and {!S.assign}; the user never types its name. *)

  type stmt
  (** A single statement. *)

  type block = stmt list
  (** A sequence of statements. *)

  type backend = GLSL | WGSL

  type program = {
    vertex : string;    (** GLSL or WGSL source for the vertex stage. *)
    fragment : string;  (** GLSL or WGSL source for the fragment stage. *)
  }

  type error =
    | Interface_mismatch of string
    (** The two stages disagree: a uniform has different types in each
        stage, or a varying has a different name in each stage. *)

  val string_of_error : error -> string

  type vertex_stage
  type fragment_stage

  type ('stage, 'inputs, 'varyings, 'outputs) builder
  (** A shader under construction. *)

  type ('stage, 'inputs, 'varyings, 'outputs) shader
  (** A finished shader. Only the body functions produce values of this type. *)

  (** {1 Type descriptors} *)

  module Ty : sig
    val float : float_s ty
    val int : int_s ty
    val uint : uint_s ty
    val bool : bool_s ty
    val vec2 : vec2 ty
    val vec3 : vec3 ty
    val vec4 : vec4 ty
    val ivec2 : ivec2 ty
    val ivec3 : ivec3 ty
    val ivec4 : ivec4 ty
    val uvec2 : uvec2 ty
    val uvec3 : uvec3 ty
    val uvec4 : uvec4 ty
    val bvec2 : bvec2 ty
    val bvec3 : bvec3 ty
    val bvec4 : bvec4 ty
    val mat2 : mat2 ty
    val mat3 : mat3 ty
    val mat4 : mat4 ty
    val sampler2d : sampler2d ty
    val sampler3d : sampler3d ty
    val sampler_cube : sampler_cube ty
    val sampler2d_shadow : sampler2d_shadow ty
  end

  (** {1 Expressions} *)

  module E : sig
    (** {2 Constants} *)

    val float : float -> float_s expr
    val int : int -> int_s expr
    val uint : int -> uint_s expr
    val bool : bool -> bool_s expr

    (** {2 Scalar arithmetic} *)

    val add_f : float_s expr -> float_s expr -> float_s expr
    val sub_f : float_s expr -> float_s expr -> float_s expr
    val mul_f : float_s expr -> float_s expr -> float_s expr
    val div_f : float_s expr -> float_s expr -> float_s expr
    val neg_f : float_s expr -> float_s expr

    val add_i : int_s expr -> int_s expr -> int_s expr
    val sub_i : int_s expr -> int_s expr -> int_s expr
    val mul_i : int_s expr -> int_s expr -> int_s expr
    val div_i : int_s expr -> int_s expr -> int_s expr

    (** {2 Comparison and logic} *)

    val lt_f : float_s expr -> float_s expr -> bool_s expr
    val le_f : float_s expr -> float_s expr -> bool_s expr
    val gt_f : float_s expr -> float_s expr -> bool_s expr
    val ge_f : float_s expr -> float_s expr -> bool_s expr
    val lt_i : int_s expr -> int_s expr -> bool_s expr
    val eq_i : int_s expr -> int_s expr -> bool_s expr
    val and_ : bool_s expr -> bool_s expr -> bool_s expr
    val or_ : bool_s expr -> bool_s expr -> bool_s expr
    val not_ : bool_s expr -> bool_s expr

    (** {2 Vector construction} *)

    val vec2 : float_s expr -> float_s expr -> vec2 expr
    val vec3 : float_s expr -> float_s expr -> float_s expr -> vec3 expr
    val vec4 :
      float_s expr -> float_s expr -> float_s expr -> float_s expr -> vec4 expr
    val vec4_of_vec3 : vec3 expr -> float_s expr -> vec4 expr

    (** {2 Vector arithmetic} *)

    val add_v2 : vec2 expr -> vec2 expr -> vec2 expr
    val scale_v2 : float_s expr -> vec2 expr -> vec2 expr
    val add_v3 : vec3 expr -> vec3 expr -> vec3 expr
    val sub_v3 : vec3 expr -> vec3 expr -> vec3 expr
    val scale_v3 : float_s expr -> vec3 expr -> vec3 expr
    val add_v4 : vec4 expr -> vec4 expr -> vec4 expr
    val scale_v4 : float_s expr -> vec4 expr -> vec4 expr

    (** {2 Swizzles} *)

    val v2x : vec2 expr -> float_s expr
    val v2y : vec2 expr -> float_s expr
    val v3x : vec3 expr -> float_s expr
    val v3y : vec3 expr -> float_s expr
    val v3z : vec3 expr -> float_s expr
    val v4x : vec4 expr -> float_s expr
    val v4y : vec4 expr -> float_s expr
    val v4z : vec4 expr -> float_s expr
    val v4w : vec4 expr -> float_s expr
    val v4xy : vec4 expr -> vec2 expr
    val v4xyz : vec4 expr -> vec3 expr

    (** {2 Geometry and matrices} *)

    val dot3 : vec3 expr -> vec3 expr -> float_s expr
    val cross3 : vec3 expr -> vec3 expr -> vec3 expr
    val length3 : vec3 expr -> float_s expr
    val normalize3 : vec3 expr -> vec3 expr
    val distance3 : vec3 expr -> vec3 expr -> float_s expr
    val reflect3 : vec3 expr -> vec3 expr -> vec3 expr
    val mat4_mul_vec4 : mat4 expr -> vec4 expr -> vec4 expr

    (** {2 Math} *)

    val abs_f : float_s expr -> float_s expr
    val floor_f : float_s expr -> float_s expr
    val fract_f : float_s expr -> float_s expr
    val sqrt : float_s expr -> float_s expr
    val pow : float_s expr -> float_s expr -> float_s expr
    val sin : float_s expr -> float_s expr
    val cos : float_s expr -> float_s expr
    val min_f : float_s expr -> float_s expr -> float_s expr
    val max_f : float_s expr -> float_s expr -> float_s expr
    val clamp_f : float_s expr -> float_s expr -> float_s expr -> float_s expr
    val mix_f : float_s expr -> float_s expr -> float_s expr -> float_s expr
    val step_f : float_s expr -> float_s expr -> float_s expr
    val smoothstep_f : float_s expr -> float_s expr -> float_s expr -> float_s expr

    (** {2 Conversions} *)

    val float_of_int : int_s expr -> float_s expr
    val int_of_float : float_s expr -> int_s expr

    (** {2 Texture sampling} *)

    val texture2d : sampler2d expr -> vec2 expr -> vec4 expr
    val texture2d_lod : sampler2d expr -> vec2 expr -> float_s expr -> vec4 expr
    val texture_cube : sampler_cube expr -> vec3 expr -> vec4 expr
  end

  (** {1 Statements}

      Blocks are ordinary lists, so statements compose freely. *)

  module S : sig
    (** [let_ ty init k] declares a fresh variable initialised to [init] and
        passes its handle to [k]. The returned block is the declaration
        followed by the statements [k] produces. *)
    val let_ : 'a ty -> 'a expr -> ('a var -> block) -> block

    val read : 'a var -> 'a expr
    val assign : 'a var -> 'a expr -> stmt

    val if_ : bool_s expr -> then_:block -> else_:block -> stmt
    val while_ : bool_s expr -> block -> stmt

    (** [for_ i start stop body] runs [body] while [i < stop], incrementing [i]
        by one. [stop] is exclusive. *)
    val for_ : int_s var -> int_s expr -> int_s expr -> block -> stmt

    val break_ : stmt
    val continue_ : stmt

    (** Valid only in fragment bodies. Not yet checked by the type system. *)
    val discard : stmt

    (** Evaluate an expression for its side effects. *)
    val eval : 'a expr -> stmt
  end

  (** {1 Stages}

      Each builder function adds one declaration. Attributes and uniforms
      accumulate in ['inputs]; varyings in ['varyings]; fragment outputs in
      ['outputs].

      [body] is terminal. It receives the declared inputs as expression
      handles, emits statements, and returns the values to assign. *)

  module Vertex : sig
    val create : unit -> (vertex_stage, unit, unit, unit) builder

    val attribute :
      string -> 'a ty ->
      (vertex_stage, 'i, 'v, unit) builder ->
      (vertex_stage, 'a * 'i, 'v, unit) builder * 'a var

    val uniform :
      string -> 'a ty ->
      (vertex_stage, 'i, 'v, unit) builder ->
      (vertex_stage, 'i, 'v, unit) builder * 'a var

    val varying :
      string -> 'a ty ->
      (vertex_stage, 'i, 'v, unit) builder ->
      (vertex_stage, 'i, 'a * 'v, unit) builder * 'a var

    (** [body b main] finishes the shader. [main] receives the
        [gl_Position] output variable and returns the body statements. *)
    val body :
      (vertex_stage, 'i, 'v, unit) builder ->
      (vec4 var -> block) ->
      (vertex_stage, 'i, 'v, unit) shader
  end

  module Fragment : sig
    val create : unit -> (fragment_stage, unit, unit, unit) builder

    val uniform :
      string -> 'a ty ->
      (fragment_stage, unit, 'v, 'o) builder ->
      (fragment_stage, unit, 'v, 'o) builder * 'a var

    val varying :
      string -> 'a ty ->
      (fragment_stage, unit, 'v, 'o) builder ->
      (fragment_stage, unit, 'a * 'v, 'o) builder * 'a var

    val output :
      string -> 'a ty ->
      (fragment_stage, unit, 'v, 'o) builder ->
      (fragment_stage, unit, 'v, 'a * 'o) builder * 'a var

    val body :
      (fragment_stage, unit, 'v, 'o) builder ->
      block ->
      (fragment_stage, unit, 'v, 'o) shader
  end

  (** {1 Compilation} *)

  module Compile : sig
    (** Generates a vertex and fragment program. The varying types are checked
        by the type system. Uniform types and varying names are checked at
        compile time, and mismatches are reported as {!Interface_mismatch}.
        The default backend is [GLSL]. *)
    val pair :
      ?backend:backend ->
      (vertex_stage, 'vi, 'v, unit) shader ->
      (fragment_stage, 'fi, 'v, 'o) shader ->
      (program, error) result
  end
