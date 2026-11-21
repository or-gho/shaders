
open Types
open Expr

(* A mutable variable reference: the type is fixed at declaration time. *)
type 'a ref_ = 'a Types.t * string

(* Statements are type-indexed by whether they "produce" a value.
   `unit stmt` = sequential/imperative; `'a stmt` = terminal (return). *)
type _ stmt =
  (* imperative *)
  | Assign   : 'a ref_ * 'a Expr.t -> unit stmt
  | Let      : 'a Types.t * string * 'a Expr.t -> unit stmt
  | Decl_var : 'a Types.t * string * 'a Expr.t -> unit stmt
  | Expr_stmt: _ Expr.t -> unit stmt
  | If       : bool_s Expr.t * unit stmt list * unit stmt list -> unit stmt
  | For      : 'a ref_ * 'a Expr.t * 'a Expr.t * unit stmt list -> unit stmt
  | While    : bool_s Expr.t * unit stmt list -> unit stmt
  | DoWhile  : unit stmt list * bool_s Expr.t -> unit stmt
  | Switch   : int_s Expr.t * (int * unit stmt list) list * unit stmt list option -> unit stmt
  | Break    : unit stmt
  | Continue : unit stmt
  | Discard  : unit stmt
  | Block    : unit stmt list -> unit stmt
  (* terminal *)
  | Return   : 'a Expr.t -> 'a stmt
