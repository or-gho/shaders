(* src/expr.ml *)
open Types

type _ expr =
  (* Constants *)
  | Float_const : float -> float expr
  | Int_const   : int -> int expr
  | Bool_const  : bool -> bool expr

  (* Variables *)
  | Var : 'a ty * string -> 'a expr

  (* Arithmetic — only float operands *)
  | Add : float expr * float expr -> float expr
  | Sub : float expr * float expr -> float expr
  | Mul : float expr * float expr -> float expr
  | Div : float expr * float expr -> float expr

  (* Vector construction *)
  | Vec2_const : float expr * float expr -> vec2 expr
  | Vec3_const : float expr * float expr * float expr -> vec3 expr
  | Vec4_const : float expr * float expr * float expr * float expr -> vec4 expr
  | Vec4_of_vec3 : vec3 expr * float expr -> vec4 expr

  (* Vector arithmetic *)
  | Vec_add : vec3 expr * vec3 expr -> vec3 expr
  | Vec_scale : float expr * vec3 expr -> vec3 expr

  (* Matrix operations *)
  | Mat_mul_vec : mat4 expr * vec4 expr -> vec4 expr

  (* Built-in functions *)
  | Dot : vec3 expr * vec3 expr -> float expr
  | Normalize : vec3 expr -> vec3 expr

  (* Swizzle: extract components *)
  | X : vec4 expr -> float expr
  | Y : vec4 expr -> float expr
  | Z : vec4 expr -> float expr
  | W : vec4 expr -> float expr
