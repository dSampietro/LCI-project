(*module RegisterSet = Set.Make(Int)*)
module RegisterSet = Register_set.RegisterSet

let lookup_table (lt: (Param_cfg.label, 'a) Hashtbl.t) (id: Param_cfg.label): 'a =
  match (Hashtbl.find_opt lt id) with
  | Some(value) -> value
  | None -> failwith "Searched for an invalid label"
  


let iteration 
  (cfg: MiniRisc_cfg.miniRisc_cfg) 
  (use_def_table: (Param_cfg.label, Use_def_table.def_use) Hashtbl.t)
  (liveness_table: Liveness_table.liveness_table): Liveness_table.liveness_table = 
  
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
          fun ({live_in=acc_in; live_out=x}: Liveness_table.liveness) (el: Liveness_table.liveness) ->
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
      
      (* get the use/def sets for the current label *)
      let use_def_label = lookup_table use_def_table label in          
      let live_in = RegisterSet.union use_def_label.use (RegisterSet.diff new_live_out use_def_label.def)
      in RegisterSet.union r_in live_in
    in Hashtbl.add lt label ({live_in=new_live_in; live_out=new_live_out}: Liveness_table.liveness)
  ) 
  in (* print_endline ("Iteration:\n" ^ show_liveness_table lt ^ "\n"); *)
  lt



let fixpoint 
  (cfg: MiniRisc_cfg.miniRisc_cfg) 
  (use_def_table: (Param_cfg.label, Use_def_table.def_use) Hashtbl.t)
  (liveness_table: Liveness_table.liveness_table): Liveness_table.liveness_table = 

  let prev_liveness_table = ref liveness_table in
  let new_liveness_table = ref (iteration cfg use_def_table !prev_liveness_table) in

  while not (Liveness_table.equal_liveness_table !new_liveness_table !prev_liveness_table) do
    prev_liveness_table := !new_liveness_table;
    new_liveness_table := iteration cfg use_def_table !prev_liveness_table;
  done;

  !new_liveness_table

let liveness_analysis (cfg: MiniRisc_cfg.miniRisc_cfg): Liveness_table.liveness_table =
  let num_nodes = List.length cfg.nodes in
  let use_def_table = Use_def_table.compute_use_def_table cfg in
  let liveness_table = Hashtbl.create num_nodes in
  
  (* initalize liveness_table with empy sets*)
  List.iter (fun (node: 'a Param_cfg.node) ->
    Hashtbl.add liveness_table node.id ({live_in = RegisterSet.empty; live_out = RegisterSet.empty}: Liveness_table.liveness)
  ) cfg.nodes;
      
  fixpoint cfg use_def_table liveness_table
