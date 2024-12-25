open Param_cfg
open MiniImp_cfg
open MiniRisc_cfg
open Translate
open Register

let map_node (reg_table: register_table) (node: miniImp_instr node) : miniRisc_instr node =
  let c1 = match node.content with
    | Statement(stmt) -> stmt_translate reg_table stmt
    | Expression(exp) -> 
      let e1, _ = exp_translate reg_table exp None in
      e1 
  in {id=node.id; content=c1}
  

let translate_cfg (g: miniImp_cfg) (input, output) : miniRisc_cfg =
  let reg_table = Register.new_table 10 in

  (* add input/output variables to register table*)
  add input 0 reg_table;
  add output 1 reg_table;
  
  let mapped_nodes = List.map (map_node reg_table) @@ List.rev g.nodes
  in {nodes=mapped_nodes; edges=g.edges}