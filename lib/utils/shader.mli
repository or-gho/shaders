type vertex_stage
type fragment_stage

type any_typ = Any : 'a Types.t -> any_typ

type ('stage, 'inputs, 'varyings, 'outputs) builder

type ('stage, 'inputs, 'varyings, 'outputs) t = {
  attrs : (any_typ * string) list;
  unifs : (any_typ * string) list;
  varys : (any_typ * string) list;
  outs : (any_typ * string) list;
  body : unit Stmt.stmt list;
}

type ('stage, 'inputs, 'varyings, 'outputs) shader =
  ('stage, 'inputs, 'varyings, 'outputs) t

val equal_any_typ : any_typ -> any_typ -> bool
val string_of_any_typ : any_typ -> string
val create_vertex : unit -> (vertex_stage, unit, unit, unit) builder
val create_fragment : unit -> (fragment_stage, unit, unit, unit) builder
val with_attribute : string -> 'a Types.t -> (vertex_stage, 'i, 'v, 'o) builder -> (vertex_stage, 'a * 'i, 'v, 'o) builder * 'a Stmt.ref_
val with_uniform : string -> 'a Types.t -> ('s, 'i, 'v, 'o) builder -> ('s, 'i, 'v, 'o) builder * 'a Stmt.ref_
val with_varying : string -> 'a Types.t -> ('s, 'i, 'v, 'o) builder -> ('s, 'i, 'a * 'v, 'o) builder * 'a Stmt.ref_
val with_output : string -> 'a Types.t -> (fragment_stage, 'i, 'v, 'o) builder -> (fragment_stage, 'i, 'v, 'a * 'o) builder * 'a Stmt.ref_
val vertex_body : (vertex_stage, 'i, 'v, unit) builder -> (Types.vec4 Stmt.ref_ -> unit Stmt.stmt list) -> (vertex_stage, 'i, 'v, unit) shader
val fragment_body : (fragment_stage, unit, 'v, 'o) builder -> unit Stmt.stmt list -> (fragment_stage, unit, 'v, 'o) shader
