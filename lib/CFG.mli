type label = int

val fresh_id: label -> (label * label)

type node = 
  | Statement of label * MiniImp.stmt
  | Expression of label * MiniImp.exp 

val get_node_id: node -> label

(* Report: minimal blocks CFG *)
type t

(* Create empty CFG *)
val empty: unit -> t

(* Add a node to the CFG *)
val add_node: t -> node -> t 

(* Add an edge to the CFG *)
val add_edge: t -> label -> label -> t 

val get_nodes: t -> node list
val get_edges: t -> (label * label) list

val get_first_node_id: t -> label 
val get_last_node_id: t -> label 
