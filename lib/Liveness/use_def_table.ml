module RegisterSet = Register_set.RegisterSet
let show_set (s: RegisterSet.t) : string =   String.concat ", " @@ List.map Register.string_of_register @@ RegisterSet.elements s 

type def_use = {
  def: RegisterSet.t;
  use: RegisterSet.t
} [@@ deriving show]

let show_use_def_table (udt: (Param_cfg.label, def_use) Hashtbl.t) = 
  Hashtbl.iter (fun label {def; use} ->
    Printf.printf "%d ->\n\tdef: %s\n\tuse: %s\n"
    label (show_set def) (show_set use)
  ) udt

let merge_sets (ls: def_use list) : def_use =
  List.fold_left 
  (fun {use=acc_use; def=acc_def} el ->
    let new_use = RegisterSet.union acc_use el.use in
    let new_def = RegisterSet.union acc_def el.def in
    {use=new_use; def=new_def}
  )  
  { use=RegisterSet.empty; def=RegisterSet.empty }
  ls


(* compute the sets of def/used regs in a single MiniRisc instruction *)
let compute_instr_use_def (e: MiniRisc.exp) : def_use =
  match e with
  | Nop -> {def=RegisterSet.empty; use=RegisterSet.empty}

  (*urop*)
  | NotR(r1, r2) | CopyR(r1, r2) -> {def=RegisterSet.singleton r2; use=RegisterSet.singleton r1}

  (*brop*)
  | AddR(r1, r2, r3) | SubR(r1, r2, r3) 
  | MultR(r1, r2, r3) | AndR(r1, r2, r3)
  | LessR(r1, r2, r3) -> {def=RegisterSet.singleton r3; use=RegisterSet.of_list [r1; r2]}

  (*biop*)
  | AddI(r1, _, r2) | SubI(r1, _, r2) 
  | MultI(r1, _, r2) | AndI(r1, _, r2) -> {def=RegisterSet.singleton r2; use=RegisterSet.singleton r1}  

  (*load/store*)
  | Load(r1, r2) -> {def=RegisterSet.singleton r2; use=RegisterSet.singleton r1}
  | LoadI(_, r2) -> {def=RegisterSet.singleton r2; use=RegisterSet.empty}
  | Store(r1, r2) -> {def=RegisterSet.singleton r2; use=RegisterSet.singleton r1}

  (*jump*)
  | Jump(_) -> {def=RegisterSet.empty; use=RegisterSet.empty}
  | CJump(r1, _, _) -> {def=RegisterSet.empty; use=RegisterSet.singleton r1}

  | _ -> {def=RegisterSet.empty; use=RegisterSet.empty}


let compute_node_use_def (e: MiniRisc_cfg.miniRisc_instr) : def_use = 
  let all_instr_def_use = List.map compute_instr_use_def e 
  in merge_sets all_instr_def_use


let compute_use_def_table (g: MiniRisc_cfg.miniRisc_cfg) : (Param_cfg.label, def_use) Hashtbl.t = 
  let ht = Hashtbl.create (List.length g.nodes) in
  let _ = List.map (
    fun (n: _ Param_cfg.node) -> 
      Hashtbl.add ht n.id (compute_node_use_def n.content)
  ) g.nodes
  in ht
