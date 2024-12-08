type label = int

(* Increment the counter and return the new ID in a functional way *)
let fresh_id id = (id + 1, id)


(* Report: to handle expression in CFG blocks => differentiate node type between Stmtm and Expr*)
type node = 
  | Statement of label * MiniImp.stmt
  | Expression of label * MiniImp.exp 


(* extract node_id from a node *)
let get_node_id (n: node): label =
  match n with
  | Statement (label, _) -> label
  | Expression (label, _) -> label

type t = {
  nodes: node list;
  edges: (label * label) list;  (* edges are pairs of label cause it is more efficient to store => not copying nodes*)
}

(* Create empty CFG *)
let empty () = {nodes=[]; edges=[]}

(* Add a node to the CFG *)
let add_node (cfg: t) (node: node) : t = 
  if List.mem node cfg.nodes then cfg 
  else { nodes = node :: cfg.nodes; edges = cfg.edges }

(* Add an (oriented) edge to the CFG *)
let add_edge (cfg: t) (id1: label) (id2: label) : t = 
  if List.mem (id1, id2) cfg.edges then cfg 
  else { nodes = cfg.nodes; edges = (id1, id2) :: cfg.edges }


let get_nodes (cfg: t) : node list = cfg.nodes
let get_edges (cfg: t) : (label * label) list = cfg.edges

let get_first_node_id (cfg: t) : label = 
  let nodes = get_nodes cfg in
  let first_node = List.hd (List.rev nodes) in
  get_node_id first_node

let get_last_node_id (cfg: t) : label = 
  let nodes = get_nodes cfg in 
  let last_node = List.hd nodes in
  get_node_id last_node