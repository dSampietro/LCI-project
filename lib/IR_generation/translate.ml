open MiniRisc

let bool_false = -1
let bool_true = 1

let get_boolean_integer (b: bool): int =
  if b then bool_true else bool_false


let check_exist_reg (reg_var: Register.register option) =
  match reg_var with 
  | Some(r) -> r
  | None -> Register.get_new_register ()

let rec exp_translate 
(env: Register.register_table) 
(e: MiniImp.exp)
(reg_var: Register.register option)
: (MiniRisc.exp list * Register.register) = 
  match e with
    | Var(x) ->
      (match (Register.lookup env x) with
      | Some(reg) -> ([], reg)
      | None -> failwith ("Var " ^ x ^ " not initialized"))

    | Aval(n) -> 
      let regDest = check_exist_reg reg_var in
      ([LoadI(n, regDest)], regDest)

    (* constant propagation*)
    | Plus(Aval(n), Aval(m)) -> 
      let regDest = check_exist_reg reg_var in
      ([LoadI(n+m, regDest)], regDest)
    
    | Plus(Var(x), Aval(n)) | Plus(Aval(n), Var(x)) -> 
      let _, reg1 = exp_translate env (Var(x)) reg_var in
      let regDest = check_exist_reg reg_var in
      ([AddI(reg1, n, regDest)], regDest)

    | Plus(e1, e2) -> 
      let code1, reg1 = exp_translate env e1 reg_var in
      let code2, reg2 = exp_translate env e2 reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ code2 @ [AddR(reg1, reg2, regDest)], regDest)


    | Minus(Aval(n), Aval(m)) -> 
      let regDest = check_exist_reg reg_var in
      ([LoadI(n-m, regDest)], regDest)

    
    (* 
    problem for x = 1 - x 
    => LoadI 1 r2
       SubR r2 r2 r2
    => collapse to 0
    
    solution: (n - x) => (-x + n) 
    *)
    | Minus(Aval(n), Var(x)) -> 
      let _, reg1 = exp_translate env (Var(x)) reg_var in
      let nr1 = Register.get_new_register () in
      let regDest = check_exist_reg reg_var in
      ([NotR(reg1, nr1) ; AddI(nr1, n, regDest)], regDest)
    
    | Minus(Var(x), Aval(n)) -> 
      let _, reg1 = exp_translate env (Var(x)) reg_var in
      let regDest = check_exist_reg reg_var in
      ([SubI(reg1, n, regDest)], regDest)

    | Minus(e1, e2) ->
      let code1, reg1 = exp_translate env e1 reg_var in
      let code2, reg2 = exp_translate env e2 reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ code2 @ [SubR(reg1, reg2, regDest)], regDest)


    | Times(Aval(n), Aval(m)) -> 
      let regDest = check_exist_reg reg_var in
      ([LoadI(n*m, regDest)], regDest)
    
    | Times(Var(x), Aval(n)) | Times(Aval(n), Var(x)) -> 
      let _, reg1 = exp_translate env (Var(x)) reg_var in
      let regDest = check_exist_reg reg_var in
      ([MultI(reg1, n, regDest)], regDest)
    
    | Times(e1, e2) ->
      let code1, reg1 = exp_translate env e1 reg_var in
      let code2, reg2 = exp_translate env e2 reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ code2 @ [MultR(reg1, reg2, regDest)], regDest)

    (* Bool *)
    | Bval(b) -> 
      let regDest = check_exist_reg reg_var in
      let value = get_boolean_integer b in 
      ( [LoadI(value, regDest)], regDest )

    (* short-circuiting AND with bool immediate *)
    | And(Bval(false), _) ->
      let regDest = check_exist_reg reg_var in
      ([LoadI(bool_false, regDest)], regDest)

    | And(Bval(true), Bval(false)) ->
      let regDest = check_exist_reg reg_var in
      ([LoadI(bool_false, regDest)], regDest)

    | And(Bval(true), Bval(true)) ->
      let regDest = check_exist_reg reg_var in
      ([LoadI(bool_true, regDest)], regDest)

    | And(e1, e2) ->
      let code1, reg1 = exp_translate env e1 reg_var in
      let code2, reg2 = exp_translate env e2 reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ code2 @ [AndR(reg1, reg2, regDest)], regDest) 
    
    (* constant folding NOT with bool immediate *)
    | Not(Bval(true)) ->
      let regDest = check_exist_reg reg_var in
      ([LoadI(bool_false, regDest)], regDest)

    | Not(Bval(false)) ->
      let regDest = check_exist_reg reg_var in
      ([LoadI(bool_true, regDest)], regDest)

    | Not(e) ->
      let code1, reg1 = exp_translate env e reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ [NotR(reg1, regDest)], regDest)


    | Minor(e1, e2) ->
      let code1, reg1 = exp_translate env e1 reg_var in
      let code2, reg2 = exp_translate env e2 reg_var in
      let regDest = check_exist_reg reg_var in
      (code1 @ code2 @ [LessR(reg1, reg2, regDest)], regDest)



let rec stmt_translate (env: Register.register_table) (s: MiniImp.stmt) : MiniRisc.exp list = 
  match s with
  | Skip -> [Nop]

  (* optimize assign of int immediates*)
  | Assign(id, Aval(n)) -> 
    let reg_var = (Register.lookup_or_add env id) in
    [LoadI(n, reg_var)]

  (* optimize assign of bool immediates*)
  | Assign(id, Bval(b)) -> 
    let reg_var = (Register.lookup_or_add env id) in
    let value = get_boolean_integer b in
    [LoadI(value, reg_var)]

  | Assign(id, Var(x)) ->
    let reg_var = (Register.lookup_or_add env id) in
    let _, reg_val = exp_translate env (Var(x)) (Some(reg_var)) in
    [CopyR(reg_val, reg_var)]

  | Assign(id, exp) ->
    let reg_var = (Register.lookup_or_add env id) in
    let code_exp, reg_val = exp_translate env exp (Some(reg_var)) in
    if reg_val = reg_var 
      then code_exp
      else code_exp @ [CopyR(reg_val, reg_var)]

  | Seq(s1, s2) ->
    let code1 = stmt_translate env s1 in
    let code2 = stmt_translate env s2 in
    code1 @ code2

  (* Simplified control flow for trivial condition *)
  | If(Bval(true), then_stmt, _) -> 
    let then_code = stmt_translate env then_stmt in
    then_code

  | If(Bval(false), _, else_stmt) -> 
    let else_code = stmt_translate env else_stmt in
    else_code


  | If(cond, then_stmt, else_stmt) -> 
    print_endline("If");
    let cond_code, cond_reg = exp_translate env cond None in

    (* use un-modified env in the branches *)
    let then_env = Hashtbl.copy env in
    let else_env = Hashtbl.copy env in
    
    let then_label = Register.get_new_label () in
    let else_label = Register.get_new_label () in
    let end_label = Register.get_new_label () in
    
    let then_code = stmt_translate then_env then_stmt in
    let else_code = stmt_translate else_env else_stmt in
    
    cond_code @
    [CJump (cond_reg, then_label, else_label)] @
    [Label then_label] @ then_code @ [Jump end_label] @ 
    [Label else_label] @ else_code @ [Label end_label]

  | While(Bval(false), _) -> []

  | While(cond, body_stmt) -> 
    let start_label = "Lstart" in
    let body_label = "Lbody" in
    let end_label = "Lend" in
    let cond_code, cond_reg = exp_translate env cond None in
    let body_code = stmt_translate env body_stmt in
    [Label start_label] @
    cond_code @
    [CJump (cond_reg, body_label, end_label)] @
    [Label body_label] @
    body_code @
    [Jump start_label; Label end_label]


let get_code_string (code: MiniRisc.exp list) : string = 
  String.concat "" (List.map MiniRisc.string_of_risc_exp code)
