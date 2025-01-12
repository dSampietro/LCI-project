type label = int

val fresh_id: unit -> label

type 'a node = { 
  id: label;
  content: 'a 
}


(* extract node_id from a node *)
val get_node_id: 'a node -> label
val compare_nodes_by_id: 'a node -> 'a node -> int

type 'a cfg = {
  nodes: 'a node list;
  edges: (label * label) list;
}

(* Create empty CFG *)
val empty: unit -> 'a cfg

(* Add a node to the CFG *)
val add_node: 'a cfg -> 'a node -> 'a cfg
val add_edge: 'a cfg -> label -> label -> 'a cfg

val remove_node_by_label: 'a cfg -> label -> 'a cfg

val get_nodes: 'a cfg -> 'a node list
val get_edges: 'a cfg -> (label * label) list
val length: 'a cfg -> int
val get_labels: 'a cfg -> label list

val predecessors: 'a cfg -> label -> label list
val successors: 'a cfg -> label -> label list 
