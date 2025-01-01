open Param_cfg

type miniImp_instr = 
  | Statement of MiniImp.stmt
  | Expression of MiniImp.exp 


type miniImp_cfg = miniImp_instr Param_cfg.cfg

let create_node content = 
  let id = fresh_id () in
  { id; content }


(* Pretty-print the CFG *)
let string_of_instr (i: miniImp_instr) : string =
  match i with
  | Statement(stmt) -> MiniImp.string_of_stmt stmt
  | Expression(exp) -> MiniImp.string_of_exp exp

let string_of_node {id; content} : string = 
  Printf.sprintf "Node %d: %s" id (string_of_instr content)

let string_of_edge (id1, id2) =
   Printf.sprintf "Edge: %d -> %d" id1 id2

let pp_cfg (g: miniImp_cfg) =
  let nodes_section = String.concat "\n" (List.map string_of_node @@ List.rev g.nodes) in
  let edges_section = String.concat "\n" (List.map string_of_edge g.edges) in
  Printf.printf "Nodes:\n%s\n\nEdges:\n%s\n" nodes_section edges_section
