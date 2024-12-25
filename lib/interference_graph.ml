type liverange = Param_cfg.label * Param_cfg.label
type liverange_table = (Register.register, liverange) Hashtbl.t


let compute_live_ranges (lt: Liveness.liveness_table): liverange_table = 
  (*helper function to update the live range*)
  let update_range reg start_id end_id ranges = 
    match Hashtbl.find_opt ranges reg with
    | None -> Hashtbl.add ranges reg (start_id, end_id)
    | Some(existing_start, existing_end) ->
      let new_start = min start_id existing_start in
      let new_end = max end_id existing_end in
      Hashtbl.replace ranges reg (new_start, new_end)
  
  in 
  let num = Hashtbl.length lt in
  let live_ranges = Hashtbl.create num in
  Hashtbl.iter(fun label (liveness_info: Liveness.liveness) ->
    let start_id = label in
    let end_id = label in

    (*update live-in regs*)
    Liveness.RegisterSet.iter (fun reg -> 
      update_range reg start_id end_id live_ranges
    ) liveness_info.live_in;

    (*update live-out regs*)
    Liveness.RegisterSet.iter (fun reg -> 
      update_range reg start_id end_id live_ranges
    ) liveness_info.live_out;

  ) lt;

  live_ranges

let show_liverange_table (lrt: liverange_table) =
  Hashtbl.iter (fun label (start_id, end_id) ->
    Printf.printf "%d ->\n\tstart: %d\n\tend: %d\n" label start_id end_id
  ) lrt



type interf_graph = Register.register Param_cfg.cfg

let build_interf_graph (lrt: liverange_table) : interf_graph = 
  let intf_graph = ref (Param_cfg.empty ()) in
  
  let registers = Hashtbl.fold 
    (fun label (_, _) acc -> label :: acc) lrt [] 
  in

  List.iter (fun reg1 ->
    List.iter (fun reg2 ->
      if reg1 <> reg2 then
        let start1, end1 = Hashtbl.find lrt reg1 in
        let start2, end2 = Hashtbl.find lrt reg2 in

        if (start1 <= end2) && (start2 <= end1) then
          intf_graph := (Param_cfg.add_edge !intf_graph reg1 reg2);
    ) registers
  ) registers;
  
  !intf_graph



(*let string_of_node {id; content} : string = 
  Printf.sprintf "Node %d: %s" id (string_of_int content)
*)
let string_of_edge (id1, id2) =
    Printf.sprintf "Edge: %d -> %d" id1 id2

let show_intf_graph (g: interf_graph) =
  (*let nodes_section = String.concat "\n" (List.map string_of_node @@ List.rev g.nodes) in*)
  let edges_section = String.concat "\n" (List.map string_of_edge g.edges) in
  Printf.printf "Edges:\n%s\n" edges_section
  