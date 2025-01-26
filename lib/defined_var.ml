module VarSet = Set.Make(String)

type defined = {
  inp: VarSet.t;
  out: VarSet.t;
}

let show_defined ({inp; out}: defined) =
  Printf.printf "IN: %s\nOUT: %s\n\n" (VarSet.elements inp |> String.concat ", ") (VarSet.elements out |> String.concat ", ")

type def_var_table = (Param_cfg.label, defined) Hashtbl.t

let show_def_var_table (table: def_var_table) =
  print_endline "\n----------\nDefined variables table\n";
  Hashtbl.iter (fun k v -> Printf.printf "Node %d\n" k; show_defined v) table;
  print_endline "----------\n"


let equal_def_var_table (dvt1: def_var_table) (dvt2: def_var_table) : bool =
  (* Ensure both tables have the same number of elements *)
  Hashtbl.length dvt1 = Hashtbl.length dvt2
  &&
  (* Compare all key-value pairs *)
  Hashtbl.fold (fun label defined acc ->
    match Hashtbl.find_opt dvt2 label with
    | Some d2 -> acc && VarSet.equal defined.inp d2.inp && VarSet.equal defined.out d2.out
    | None -> false
  ) dvt1 true


let rec used_by_exp (exp: MiniImp.exp) : VarSet.t =
  match exp with
  | Var(name)         -> VarSet.singleton (name)
  | Aval(_) | Bval(_) -> VarSet.empty

  | Plus (e1, e2)  -> VarSet.union (used_by_exp e1) (used_by_exp e2)
  | Minus (e1, e2) -> VarSet.union (used_by_exp e1) (used_by_exp e2)
  | Times (e1, e2) -> VarSet.union (used_by_exp e1) (used_by_exp e2)
  | And (e1, e2)   -> VarSet.union (used_by_exp e1) (used_by_exp e2)
  | Not(e)         -> (used_by_exp e)
  | Minor (e1, e2) -> VarSet.union (used_by_exp e1) (used_by_exp e2)
  
let rec used_by_stmt (st: MiniImp.stmt) : VarSet.t =
  match st with
  | Skip                -> VarSet.empty
  | Assign (_, exp)     -> (used_by_exp exp)
  | Seq (s1, s2)        -> VarSet.union (used_by_stmt s1) (used_by_stmt s2)
  | If (cond, s1, s2)   -> VarSet.union (used_by_exp cond) @@ VarSet.union (used_by_stmt s1) (used_by_stmt s2)
  | While (cond, body)  -> VarSet.union (used_by_exp cond) (used_by_stmt body)


let defined_by_stmt (st: MiniImp.stmt) : string list =
  match st with
  | Assign (var, _) -> [var]
  | _ -> []


type node = MiniImp_cfg.miniImp_instr Param_cfg.node






(* forward analysis: start from label 0 *)
(* MiniImp cfg is minimal: instruction is one of assignement or use*)
let iteration 
(g: MiniImp_cfg.miniImp_cfg)
(def_var_table: def_var_table)
(input: string) : def_var_table =
  let nodes = (List.rev g.nodes) in 
  let dvt = Hashtbl.create (List.length nodes) in
  
  List.iter (fun ({id=id; content=content}: node) ->
    let inp =
      (*if id = 0 then VarSet.singleton input
      else
        (* intersection between predecessors output *)
        let pred = Param_cfg.predecessors g id in
        
        let p1 = List.hd pred in
        let initial_acc = (Hashtbl.find def_var_table p1).out in
        let pred = List.tl pred in

        List.fold_left 
          (fun acc p -> VarSet.inter acc (Hashtbl.find def_var_table p).out) 
          initial_acc 
          pred
      *)
      let pred = Param_cfg.predecessors g id in
      match pred with
      | [] -> VarSet.singleton input    (* only first node has no predecessors *)
      | _ ->           
        let p1 = List.hd pred in
        let initial_acc = (Hashtbl.find def_var_table p1).out in

        List.fold_left 
          (fun acc p -> VarSet.inter acc (Hashtbl.find def_var_table p).out) 
          initial_acc 
          (List.tl pred)
    in 
    (* assert (used (node.content)) € node.inp U defined(node.content)*)
    
    let out = 
      let defined = 
        match content with
        | Statement(stmt) -> defined_by_stmt stmt
        | Expression(_) -> [] 
      in
      VarSet.union inp (VarSet.of_list defined)
    
    in Hashtbl.replace dvt id {inp; out}
  ) nodes;
  
  dvt
  
  (*
  iterate until fixpoint
  - (pred node) |> out |> merge
  - node.out = node.inp U defined(node.content)

  when def_var_table is stable, then
  - assert (used (node.content)) € node.inp U defined(node.content)
  *)


let fixpoint (g: MiniImp_cfg.miniImp_cfg) (def_var_table: def_var_table) (input: string) : def_var_table =
  let prev_def_var_table = ref def_var_table in
  let new_def_var_table = ref (iteration g !prev_def_var_table input) in

  while not (equal_def_var_table !new_def_var_table !prev_def_var_table) do
    print_endline @@ "New == Prev:" ^ string_of_bool (equal_def_var_table !new_def_var_table !prev_def_var_table);
    (*show_def_var_table !new_def_var_table;*)

    prev_def_var_table := !new_def_var_table;
    new_def_var_table := iteration g !prev_def_var_table input;
  done;

  !new_def_var_table


let defined_vars_analysis (g: MiniImp_cfg.miniImp_cfg) (input: string) =
  let nodes = (List.rev g.nodes) in 
  let def_var_table = Hashtbl.create (List.length nodes) in

  (* initialize def_var_table with empty *)
  List.iter (fun ({id=id; _}: node) ->
    Hashtbl.add def_var_table id {inp=VarSet.empty; out=VarSet.empty}
  ) nodes;

  let dvt = fixpoint g def_var_table input in 

  (* assert all instructions use defined variables*)
  List.iter (fun ({id=id; content=content}: node) ->
    let used_vars = 
      match content with
      | Statement(stmt) -> used_by_stmt stmt
      | Expression(exp) -> used_by_exp exp
    in

    let defined_vars = (Hashtbl.find dvt id).out 
    in

    if not (VarSet.subset used_vars defined_vars) 
      then failwith @@ "Node " ^ (string_of_int id) ^ " is using an undefined variable"


  ) nodes;
