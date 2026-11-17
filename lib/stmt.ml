open Types
open Expr

type _ stmt =
  | Assign : 'a ref * 'a expr -> unit stmt
  | Let : 'a ty * string * 'a expr -> unit stmt
  | If : bool expr * unit stmt list * unit stmt list -> unit stmt
  | Return : 'a expr -> unit stmt
  | Expr_stmt : unit expr -> unit stmt

(* A mutable variable reference *)
and 'a ref = 'a ty * string
