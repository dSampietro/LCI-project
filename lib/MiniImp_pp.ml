let rec string_of_exp (exp: MiniImp.exp) : string =
  match exp with
  | Var name -> Printf.sprintf "Var(%s)" name
  | Aval n -> Printf.sprintf "Aval(%d)" n
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Times (e1, e2) -> Printf.sprintf "Times(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Bval b -> Printf.sprintf "Bval(%b)" b
  | And (e1, e2) -> Printf.sprintf "And(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Not e -> Printf.sprintf "Not(%s)" (string_of_exp e)
  | Minor (e1, e2) -> Printf.sprintf "Minor(%s, %s)" (string_of_exp e1) (string_of_exp e2)


let string_of_stmt (stmt: MiniImp.stmt) : string =
  match stmt with
  | Skip -> "Skip"
  | Assign (var, exp) -> Printf.sprintf "Assign(%s, %s)" var (string_of_exp exp)
  | Seq (_, _) -> "Seq" (* Not printing the full sequence here for simplicity *)
  | If (_, _, _) -> "If"
  | While (_, _) -> "While"