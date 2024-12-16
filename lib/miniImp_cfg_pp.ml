open Param_cfg
open MiniImp_cfg

(* Pretty-print the CFG *)


let string_of_instr (i: miniImp_instr) : string =
  match i with
  | Statement(stmt) -> MiniImp_pp.string_of_stmt stmt
  | Expression(exp) -> MiniImp_pp.string_of_exp exp

let string_of_node {id; content} : string = 
  Printf.sprintf "Node %d: %s" id (string_of_instr content)

let string_of_edge (id1, id2) =
   Printf.sprintf "Edge: %d -> %d" id1 id2

let pp_cfg (g: miniImp_cfg) =
  let nodes_section = String.concat "\n" (List.map string_of_node @@ List.rev g.nodes) in
  let edges_section = String.concat "\n" (List.map string_of_edge g.edges) in
  Printf.printf "Nodes:\n%s\n\nEdges:\n%s\n" nodes_section edges_section
