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
let rec string_of_exp (e: exp) : string =
  match e with
  | Var name -> Printf.sprintf "Var(%s)" name
  | Aval n -> Printf.sprintf "Aval(%d)" n
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Times (e1, e2) -> Printf.sprintf "Times(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Bval b -> Printf.sprintf "Bval(%b)" b
  | And (e1, e2) -> Printf.sprintf "And(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Not e -> Printf.sprintf "Not(%s)" (string_of_exp e)
  | Minor (e1, e2) -> Printf.sprintf "Minor(%s, %s)" (string_of_exp e1) (string_of_exp e2)


let string_of_stmt (st: stmt) : string =
  match st with
  | Skip -> "Skip"
  | Assign (var, exp) -> Printf.sprintf "Assign(%s, %s)" var (string_of_exp exp)
  | Seq (_, _) -> "Seq" (* Not printing the full sequence here for simplicity *)
  | If (_, _, _) -> "If"
  | While (_, _) -> "While"