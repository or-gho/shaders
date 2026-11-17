(* src/shader.ml *)
open Types
open Expr
open Stmt

(* Vertex shader: attributes in, varyings out, writes gl_Position *)
type ('attrs,'uniforms, 'varyings) vertex_shader =
  { vs_attributes : 'attrs
  ; vs_uniforms   : 'uniforms
  ; vs_varyings   : 'varyings
  ; vs_body       : unit stmt list
  }

(* Fragment shader: varyings in, one colour out *)
type ('uniforms, 'varyings, 'output) fragment_shader =
  { fs_uniforms : 'uniforms
  ; fs_varyings : 'varyings
  ; fs_output   : 'output ty * string
  ; fs_body     : unit stmt list
  }
