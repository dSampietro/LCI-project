module RegisterSet = Set.Make(Int)

type def_use = {
  def: RegisterSet.t;
  use: RegisterSet.t
} [@@ deriving show]


type liveness = {
  live_in: RegisterSet.t;
  live_out: RegisterSet.t
} [@@ deriving show]


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
  | LoadI(_, r2) -> {def=RegisterSet.empty; use=RegisterSet.singleton r2}
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


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t

let lookup_table (lt: (Param_cfg.label, 'a) Hashtbl.t) (id: Param_cfg.label): 'a =
  match (Hashtbl.find_opt lt id) with
  | Some(value) -> value
  | None -> failwith "Search for an invalid label"







let fixpoint 
  (cfg: MiniRisc_cfg.miniRisc_cfg) 
  (use_def_table: (Param_cfg.label, def_use) Hashtbl.t)
  (liveness_table: liveness_table): liveness_table = 
  
  let num_nodes = Param_cfg.length cfg in 
  let labels = Param_cfg.get_labels cfg in
  let lt = Hashtbl.create num_nodes in
  
  let _ = labels |> 
  List.map (fun label ->
    let new_live_out = 
      if(label==num_nodes-1) then RegisterSet.singleton 1 (* r_out *)
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
      if(label==0) then RegisterSet.singleton 0 (* r_in *)
      else 
        let use_def_label = lookup_table use_def_table label in     (* get the use/def sets for the current label *)
        let out_label = (lookup_table liveness_table label).live_out in  (* get the live_out set for the current label *)
        RegisterSet.union use_def_label.use (RegisterSet.diff out_label use_def_label.def)
    in Hashtbl.add lt label {live_in=new_live_in; live_out=new_live_out}
  ) 
  in lt



let liveness_analysis (cfg: MiniRisc_cfg.miniRisc_cfg): liveness_table =
  let num_nodes = List.length cfg.nodes in
  let use_def_table = compute_use_def_table cfg in
  let liveness_table = Hashtbl.create num_nodes in
  
  (* initalize liveness_table with empy sets*)
  List.iter (fun (node: 'a Param_cfg.node) ->
    Hashtbl.add liveness_table node.id {live_in = RegisterSet.empty; live_out = RegisterSet.empty}
  ) cfg.nodes;
  (* backward analysis: from end to beginning; compute out then in*)
    
  (* should be fixpoint *)
  let _ = (Param_cfg.get_labels cfg) |> 
  List.map (fun label ->
    let new_live_out = 
      if(label==num_nodes-1) then RegisterSet.singleton 1 (* r_out *)
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
      if(label==0) then RegisterSet.singleton 0 (* r_in *)
      else 
        let use_def_label = lookup_table use_def_table label in     (* get the use/def sets for the current label *)
        let out_label = (lookup_table liveness_table label).live_out in  (* get the live_out set for the current label *)
        RegisterSet.union use_def_label.use (RegisterSet.diff out_label use_def_label.def)
    in Hashtbl.add liveness_table label {live_in=new_live_in; live_out=new_live_out}
  )
in liveness_table
