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

type program =
  | Main of string * string * stmt


(* use for pretty-printing *)
let rec stmt_to_string (s: stmt) : string =
  match s with
  | Skip -> "skip"
  | Assign (id, e) -> id ^ " := " ^ exp_to_string e
  | Seq (s1, s2) -> stmt_to_string s1 ^ "; " ^ stmt_to_string s2
  | If (cond, then_branch, else_branch) ->
    "if " ^ exp_to_string cond ^ " then { " ^ stmt_to_string then_branch ^ " } else { " ^ stmt_to_string else_branch ^ " }"
  | While (cond, body) ->
    "while " ^ exp_to_string cond ^ " do { " ^ stmt_to_string body ^ " }"

and exp_to_string (e: exp) : string =
  match e with
  | Var id -> id
  | Aval n -> string_of_int n
  | Plus (e1, e2) -> "(" ^ exp_to_string e1 ^ " + " ^ exp_to_string e2 ^ ")"
  | Minus (e1, e2) -> "(" ^ exp_to_string e1 ^ " - " ^ exp_to_string e2 ^ ")"
  | Times (e1, e2) -> "(" ^ exp_to_string e1 ^ " * " ^ exp_to_string e2 ^ ")"
  | Bval b -> string_of_bool b
  | And (e1, e2) -> "(" ^ exp_to_string e1 ^ " && " ^ exp_to_string e2 ^ ")"
  | Not e -> "not (" ^ exp_to_string e ^ ")"
  | Minor (e1, e2) -> "(" ^ exp_to_string e1 ^ " < " ^ exp_to_string e2 ^ ")"