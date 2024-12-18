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


type 'a cfg = {
  nodes: 'a node list;
  edges: (label * label) list;
}

(* Create empty CFG *)
let empty () = {nodes=[]; edges=[]}

(* Add a node to the CFG *)
let add_node (cfg: 'a cfg) (node: 'a node) : 'a cfg = 
  if List.mem node cfg.nodes then cfg 
  else { nodes = node :: cfg.nodes; edges = cfg.edges }

(* Add an (oriented) edge to the CFG *)
let add_edge (cfg: 'a cfg) (id1: label) (id2: label) : 'a cfg = 
  if List.mem (id1, id2) cfg.edges then cfg 
  else { nodes = cfg.nodes; edges = (id1, id2) :: cfg.edges }


let get_nodes (cfg: 'a cfg) : 'a node list = cfg.nodes
let get_edges (cfg: 'a cfg) : (label * label) list = cfg.edges

let length (cfg: 'a cfg) : int = List.length (get_nodes cfg)