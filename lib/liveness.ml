module RegisterSet = Set.Make(Int)

type def_use = {
  def: RegisterSet.t;
  use: RegisterSet.t
} [@@ deriving show]


type liveness = {
  live_in: RegisterSet.t;
  live_out: RegisterSet.t
} [@@ deriving show]

let show_set (s: RegisterSet.t) : string = 
  String.concat ", " @@ List.map Register.string_of_register @@ RegisterSet.elements s 

let show_liveness (liveness: liveness): string = 
  let in_str =  show_set liveness.live_in in
  let out_str = show_set liveness.live_out in
  "\tlive_in: " ^ in_str ^ "\n\tlive_out: " ^ out_str 


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t [@@ deriving show]

let lookup_table (lt: (Param_cfg.label, 'a) Hashtbl.t) (id: Param_cfg.label): 'a =
  match (Hashtbl.find_opt lt id) with
  | Some(value) -> value
  | None -> failwith "Searched for an invalid label"
  

let show_liveness_table (table: liveness_table) : string =
  Hashtbl.fold (fun label liveness acc ->
    let label_str = string_of_int label in
    let liveness_str = show_liveness liveness in
    acc ^ label_str ^ " ->\n" ^ liveness_str ^ "\n"
  ) table ""

let equal_liveness_table lt1 lt2 =
  Hashtbl.fold (fun label liveness acc ->
    acc && (Hashtbl.find_opt lt2 label = Some liveness)
  ) lt1 true
  && Hashtbl.fold (fun label liveness acc ->
    acc && (Hashtbl.find_opt lt1 label = Some liveness)
  ) lt2 true


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

let show_use_def_table (udt: (Param_cfg.label, def_use) Hashtbl.t) = 
  Hashtbl.iter (fun label {def; use} ->
    Printf.printf "%d ->\n\tdef: %s\n\tuse: %s\n"
    label (show_set def) (show_set use)
  ) udt

let iteration 
  (cfg: MiniRisc_cfg.miniRisc_cfg) 
  (use_def_table: (Param_cfg.label, def_use) Hashtbl.t)
  (liveness_table: liveness_table): liveness_table = 
  
  let num_nodes = Param_cfg.length cfg in 
  let labels = Param_cfg.get_labels cfg in
  let lt = Hashtbl.create num_nodes in

  (* backward analysis: from end to beginning; compute out then in*)
  let _ = List.rev labels |> 
  List.map (fun label ->
    let new_live_out = 
      if(label = num_nodes-1) then RegisterSet.singleton 1 (* r_out *)
      else (*List.fold Set.union [in[m] for m in (successor cfg label)] *)
        let x = (Param_cfg.successors cfg label)
        |> List.map (lookup_table liveness_table)
        |> List.fold_left (
          fun {live_in=acc_in; live_out=x} el ->
            let new_in = RegisterSet.union acc_in el.live_in in
            {live_in=new_in; live_out=x}      
        ) {live_in=RegisterSet.empty; live_out=RegisterSet.empty}
        in x.live_in
    in
    let new_live_in = 
      let r_in =
        if(label = 0) then RegisterSet.singleton 0
        else RegisterSet.empty
      in
      
      let use_def_label = lookup_table use_def_table label in          (* get the use/def sets for the current label *)
      let live_in = RegisterSet.union use_def_label.use (RegisterSet.diff new_live_out use_def_label.def) (* maybe new_live_out instead of old live_out *)
      in RegisterSet.union r_in live_in
    in Hashtbl.add lt label {live_in=new_live_in; live_out=new_live_out}
  ) 
  in (* print_endline ("Iteration:\n" ^ show_liveness_table lt ^ "\n"); *)
  lt



let fixpoint 
  (cfg: MiniRisc_cfg.miniRisc_cfg) 
  (use_def_table: (Param_cfg.label, def_use) Hashtbl.t)
  (liveness_table: liveness_table): liveness_table = 

  let prev_liveness_table = ref liveness_table in
  let new_liveness_table = ref (iteration cfg use_def_table !prev_liveness_table) in

  while not (equal_liveness_table !new_liveness_table !prev_liveness_table) do
    prev_liveness_table := !new_liveness_table;
    new_liveness_table := iteration cfg use_def_table !prev_liveness_table;
  done;

  !new_liveness_table

let liveness_analysis (cfg: MiniRisc_cfg.miniRisc_cfg): liveness_table =
  let num_nodes = List.length cfg.nodes in
  let use_def_table = compute_use_def_table cfg in
  let liveness_table = Hashtbl.create num_nodes in
  
  (* initalize liveness_table with empy sets*)
  List.iter (fun (node: 'a Param_cfg.node) ->
    Hashtbl.add liveness_table node.id {live_in = RegisterSet.empty; live_out = RegisterSet.empty}
  ) cfg.nodes;
      
  fixpoint cfg use_def_table liveness_table
