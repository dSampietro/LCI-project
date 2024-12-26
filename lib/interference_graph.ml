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

  (* add nodes to the interf_graph*)
  let label = ref 0 in
  List.iter (fun reg -> 
    let (node: Register.register Param_cfg.node) = {id = !label; content = reg} in
    intf_graph := Param_cfg.add_node !intf_graph node;
    incr label
  ) registers;

  (* add edges to the interf_graph*)
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
  Printf.printf "#nodes: %d\n" (List.length @@ Param_cfg.get_labels g);
  Printf.printf "Edges:\n%s\n" edges_section
  

(* coloring *)
(*
- Brook's theorem:
in a connected graph in which every vertex has at most Δ neighbors, 
the vertices can be colored with only Δ colors,
except for complete graphs (#edges = n(n-1)/2) and cycle graphs of odd length, 
which require Δ + 1 colors. 
*)

type degree = int
type degree_table = (Param_cfg.label, degree) Hashtbl.t

(* get the nieghbors of a node n in an undirected graph g*)
let get_neighbors (g: interf_graph) (n: Param_cfg.label): Param_cfg.label list =
  let preds = Param_cfg.predecessors g n in
  let succs = Param_cfg.successors g n in
  List.sort_uniq compare (preds @ succs)


let get_degree_table (g: interf_graph) : degree_table = 
  let labels = Param_cfg.get_labels g in
  let table = Hashtbl.create (List.length labels) in
  
  
  List.iter ( fun label -> 
    let num_neigh = List.length @@ get_neighbors g label in
    Hashtbl.add table label num_neigh
  ) labels;

  table

let show_degree_table (nt: degree_table) =
  print_endline ("\n\nNeighbor table\n");
  Printf.printf "Size: %d\n" (Hashtbl.length nt);
  Hashtbl.iter ( fun label num ->
    Printf.printf "\t%d: %d\n" label num
  ) nt;



(*
REGISTER ALLOCATION
- assume uniform spilling cost
- use Chaitin-Briggs algorithm w/ optimistic coloring
*)

type color = int
type color_table = (Register.register, color) Hashtbl.t

let show_color_table (ct: color_table) = 
  print_endline "Color table\n";
  Hashtbl.iter (fun reg col ->
    Printf.printf "reg: %d\tcol: %d\n" reg col
  ) ct

type address = int

type content = int
type memory = (address, content) Hashtbl.t

type spill_table = (Register.register, address) Hashtbl.t

(*
let kcoloring (g: interf_graph) (k: int) : color_table * spill_table = 
  let stack = Stack.create () in
  let degree_table = (get_degree_table g) in

  let num_nodes = List.length @@ Param_cfg.get_labels g in
  let (color_table: color_table) = Hashtbl.create num_nodes in
  let (spill_table: spill_table) = Hashtbl.create num_nodes in
  
  (* Node selection *)
  let simplify () = 
    (* find the node with degree <= k *)
    let (candidates: Param_cfg.label list) = Hashtbl.fold (
      fun label degree acc ->
        if degree <= k 
          then label :: acc
          else acc
    ) degree_table [] in

    (* if there are multiple nodes that can be removed; select the one with the largest label*)
    let label_to_remove = List.fold_left max (List.hd candidates) (List.tl candidates) in
    Printf.printf "remove: %d\n" label_to_remove;
    (* Remove the node from the graph & and push to stack*)
    Stack.push label_to_remove stack;
    let new_graph = Param_cfg.remove_node_by_label g label_to_remove in

    (* update degree table *)
    let _degree_table = get_degree_table new_graph in () 
    (* return the deg_table? *)
    
  in

  (* Graph coloring *)
  let color_node label =
    let neighbor_colors =
      List.fold_left (fun acc neighbor ->
        match Hashtbl.find_opt color_table neighbor with
        | Some(color) -> color :: acc
        | None -> acc
        ) [] (get_neighbors g label)
    in
    let rec find_available_color (color: color): color =
      if List.mem color neighbor_colors then
        find_available_color (color + 1)
      else color
    in 
    let color = find_available_color 0 in
    Hashtbl.replace color_table label color
  in

  
  (* Assign color to nodes in stack *)
  let assign_colors () = 
    while not (Stack.is_empty stack) do
      let node = Stack.pop stack in
      color_node node    
    done
  in

  (* Handle spilling *)
  let handle_spill () = 
    List.iter (fun label ->
      (* if the node is not colored => spill *)
      if not (Hashtbl.mem color_table label) then 
        let address = 1000 in
        (* keep track of spilled nodes *)
        Hashtbl.add spill_table label address;
        (*Hashtbl.add memory address _;*)
    ) (Param_cfg.get_labels g) in

  (* main loop *)
  let rec loop () =
    if num_nodes > 0 then (
      simplify ();
      loop ()
    ) else (
      assign_colors ();
      handle_spill ()
    )
  in

  loop ();
  color_table, spill_table

*)


let kcoloring (g: interf_graph) (k: int) : color_table * spill_table = 
  let stack = Stack.create () in

  let num_nodes = List.length @@ Param_cfg.get_labels g in
  let (color_table: color_table) = Hashtbl.create num_nodes in
  let (spill_table: spill_table) = Hashtbl.create num_nodes in
  
  (* Node selection *)
  (* Report: if there are multiple labels to choose => prefer the one with highest id 
    => the lower one are higher in the stack so they receive lower color *)
  let label_to_remove graph degree_table: Param_cfg.label option =
    let (candidates: Param_cfg.label list) = List.filter (fun label -> (Hashtbl.find degree_table label) <= k) (Param_cfg.get_labels graph) in
    match candidates with
    | [] -> None
    | _ -> Some(List.fold_left max (List.hd candidates) (List.tl candidates))
  in

  let rec loop (graph: interf_graph) (degree_table: degree_table) =
    match Param_cfg.get_labels graph with
    | [] -> ()  (* empty graph*)
    | labels ->
      match label_to_remove graph degree_table with
      | Some(node_to_remove) ->
        (* push node to stack *)
        Stack.push node_to_remove stack;
        
        (* remove node from graph and update *)
        let new_graph = Param_cfg.remove_node_by_label graph node_to_remove in
        let new_degree_table = get_degree_table new_graph in
        loop new_graph new_degree_table
      
      | None ->
        (* Spill: choose a node to spill (pick the first available node) *)
        let node_to_spill = 
          match labels with 
          | [] -> failwith "No nodes left to spill"
          | hd :: _ -> hd
        in
        
        (*TODO: add to memory*)
        let address = 0 in
        Hashtbl.add spill_table node_to_spill address;

        let new_graph = Param_cfg.remove_node_by_label graph node_to_spill in
        let new_degree_table = get_degree_table new_graph in
        loop new_graph new_degree_table
  in

  let degree_table = get_degree_table g in
  loop g degree_table;

  (* Assign colors *)
  while not (Stack.is_empty stack) do
    let label = Stack.pop stack in

    (* get color of all neighbors *)
    let neighbor_colors = List.fold_left ( fun acc neigh ->
      match Hashtbl.find_opt color_table neigh with
      | Some(col) -> col :: acc
      | None -> acc
    ) [] (get_neighbors g label)

    in
    (* assign lowest available color*)
    let rec find_available_color color =
      if List.mem color neighbor_colors then
        find_available_color (color+1)
      else
        color
    in
    let assigned_color = find_available_color 0 in
    Hashtbl.add color_table label assigned_color
  done;

  (* Return the color and spill tables *)
  color_table, spill_table