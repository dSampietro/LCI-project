open MiniImp

(* Define the type for memory as a map from strings (variable names) to values *)
module StringMap = Map.Make(String)

type memory = value StringMap.t

(* Lookup the value of a variable in memory *)
let lookup (mem: memory) (var: string) : value option =
  StringMap.find_opt var mem

(* Update the value of a variable in memory *)
let update (mem: memory) (id: string) (v: value) : memory =
  StringMap.add id v mem



let rec eval_exp (mem: memory) (e: exp): value = 
  match e with
  | Var s -> 
    (match lookup mem s with
      | Some value -> value
      | None -> failwith ("Variable " ^ s ^ " not found"))
  | Aval n -> IntVal n
  | Plus (a, b) -> 
    (match (eval_exp mem a, eval_exp mem b) with
      | (IntVal x, IntVal y) -> IntVal(x + y)
      | _ -> failwith "Type error")
  | Minus (a, b) -> 
    (match (eval_exp mem a, eval_exp mem b) with
      | (IntVal x, IntVal y) -> IntVal(x - y)
      | _ -> failwith "Type error")
  | Times (a, b) -> 
    (match (eval_exp mem a, eval_exp mem b) with
      | (IntVal x, IntVal y) -> IntVal(x * y)
      | _ -> failwith "Type error")

  | Bval b -> BoolVal b
  | And (a, b) -> 
    (match (eval_exp mem a, eval_exp mem b) with
      | (BoolVal x, BoolVal y) -> BoolVal(x && y)
      | _ -> failwith "Type error")    
  | Not b -> 
    (match (eval_exp mem b) with
      | BoolVal b -> BoolVal(not b)
      | _ -> failwith "Type error")
  | Minor (a, b) -> 
    (match (eval_exp mem a, eval_exp mem b) with
      | (IntVal x, IntVal y) -> BoolVal(x < y)
      | _ -> failwith "Type error")


(* Evaluate a statement, returning the updated memory *)
let rec eval_stmt (mem: memory) (s: stmt) : memory = 
  match s with
  | Skip -> mem

  | Assign(id, e) -> 
    let value = eval_exp mem e 
    in update mem id value

  | Seq(c1, c2) -> 
    let mem' = eval_stmt mem c1
    in eval_stmt mem' c2

  | If(b, c1, c2) -> 
    (match(eval_exp mem b) with
      | BoolVal(true) -> eval_stmt mem c1
      | BoolVal(false) -> eval_stmt mem c2
      | _ -> failwith "Type error in condition")

  | While(b, c) -> 
    let rec loop (mem: memory) = 
      (match(eval_exp mem b) with
        | BoolVal(true) -> 
            let mem' = eval_stmt mem c
            in loop mem' 
        | BoolVal(false) -> mem
        | _ -> failwith "Type error in condition")
    in loop mem
