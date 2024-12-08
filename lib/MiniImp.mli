(* Define a type for values, which can be either int or bool *)

type value =
  | IntVal of int
  | BoolVal of bool

type exp =
  | Var of string
  (* Int *)
  | Aval of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  (* Bool *)
  | Bval of bool
  | And of exp * exp
  | Not of exp
  | Minor of exp * exp


type stmt =
  | Skip
  | Assign of string * exp  (* Variable assignment *)
  | Seq of stmt * stmt       (* Sequence of statements *)
  | If of exp * stmt * stmt (* If-else statement *)
  | While of exp * stmt 


val stmt_to_string : stmt -> string
val exp_to_string : exp -> string
