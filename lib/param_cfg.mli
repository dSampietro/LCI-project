type label = int

val fresh_id: unit -> label

type 'a node = { 
  id: label;
  content: 'a 
}


(* extract node_id from a node *)
val get_node_id: 'a node -> label

type 'a cfg = {
  nodes: 'a node list;
  edges: (label * label) list;
}

(* Create empty CFG *)
val empty: unit -> 'a cfg

(* Add a node to the CFG *)
val add_node: 'a cfg -> 'a node -> 'a cfg
val add_edge: 'a cfg -> label -> label -> 'a cfg

val get_nodes: 'a cfg -> 'a node list
val get_edges: 'a cfg -> (label * label) list
val get_first_node_id: 'a cfg -> label
val get_last_node_id: 'a cfg -> label