(* Report: label always start from 0 *)

type label = int

let label_counter = ref(-1)
let fresh_id (): int = 
  incr label_counter;
  !label_counter


type 'a node = { 
  id: label;
  content: 'a 
}


(* extract node_id from a node *)
let get_node_id (n: 'a node): label = n.id

let compare_nodes_by_id (node1: 'a node) (node2: 'a node) =
  compare node1.id node2.id


type 'a cfg = {
  nodes: 'a node list;
  edges: (label * label) list;
}

(* Create empty CFG *)
let empty () = {nodes=[]; edges=[]}


(** Add a node to the CFG **)
let add_node (cfg: 'a cfg) (node: 'a node) : 'a cfg = 
  match List.mem node cfg.nodes with
  | true -> cfg
  | false -> { nodes = node::cfg.nodes; edges = cfg.edges } 
  (* because of recursive definiton of lists, labels are put in reversed order*)

(* Add an (oriented) edge to the CFG *)
let add_edge (cfg: 'a cfg) (id1: label) (id2: label) : 'a cfg = 
  match List.mem (id1, id2) cfg.edges with
  | true -> cfg
  | false -> { nodes = cfg.nodes; edges = (id1, id2)::cfg.edges }


let remove_node_by_label (cfg: 'a cfg) (id: label) : 'a cfg =
  (* remove node with label *)
  let updated_nodes = List.filter (fun n -> n.id <> id) cfg.nodes in
  
  (* remove edges containing label *)
  let updated_edges = List.filter ( fun (src, dest) ->
    src <> id && dest <> id
  ) cfg.edges in
  
  {nodes = updated_nodes; edges = updated_edges}
  

let get_nodes (cfg: 'a cfg) : 'a node list = cfg.nodes
let get_edges (cfg: 'a cfg) : (label * label) list = cfg.edges

let length (cfg: 'a cfg) : int = List.length (get_nodes cfg)

let get_labels (cfg: 'a cfg) : label list =
  cfg.nodes
  |> List.map (fun (n: 'a node) -> n.id) 

let predecessors (cfg: 'a cfg) (id: label) : label list = 
  cfg.edges 
  |> List.filter (fun (_, x) -> x==id) 
  |> List.map fst

let successors (cfg: 'a cfg) (id: label) : label list = 
  cfg.edges 
  |> List.filter (fun (x, _) -> x==id) 
  |> List.map snd