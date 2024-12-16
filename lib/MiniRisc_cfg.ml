open Param_cfg

type miniRisc_instr = MiniRisc.exp list 


type miniRisc_cfg = miniRisc_instr Param_cfg.cfg

let create_node content = 
  let id = fresh_id () in
  { id; content }



let string_of_node {id; content} : string = 
  let content_string = String.concat "" (List.map MiniRisc_pp.string_of_risc_exp content) in
  Printf.sprintf "Node %d:\n%s" id content_string
   
let string_of_edge (id1, id2) =
  Printf.sprintf "Edge: %d -> %d" id1 id2
 
let pp_cfg (g: miniRisc_cfg) =
  let nodes_section = String.concat "\n" (List.map string_of_node g.nodes) in
  let edges_section = String.concat "\n" (List.map string_of_edge g.edges) in
  Printf.printf "Nodes:\n%s\n\nEdges:\n%s\n" nodes_section edges_section
  