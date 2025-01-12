open Param_cfg
open MiniImp_cfg
open MiniRisc_cfg
open Translate
open Register

let map_node (reg_table: register_table) (node: miniImp_instr node) : miniRisc_instr node =
  let c1 = match node.content with
    | Statement(stmt) -> stmt_translate reg_table stmt
    | Expression(exp) -> 
      let e1, rdest = exp_translate reg_table exp None in
      e1 @ [MiniRisc.CJump(rdest, "_", "_")]
  in {id=node.id; content=c1}
  
(* add CJump to nodes with exactly 2 successors *)
let fix_cjumps (g: miniRisc_cfg) : miniRisc_cfg =
  let nodes = g.nodes in

  let mapped_nodes = List.map (fun {id=id; content=c} ->
    let updated_content = List.map (fun instr ->
      match instr with
      | MiniRisc.CJump(rd, _, _) ->
        (match List.rev (Param_cfg.successors g id) with
        | [s1; s2] -> MiniRisc.CJump(rd, string_of_int s1, string_of_int s2)
        | _ -> failwith ("Error: Node " ^ string_of_int id ^ " does not have exactly two successors for CJump"))
      | _ -> instr) c 
    in {id=id; content=updated_content} 
  ) nodes
  
  in {nodes=mapped_nodes; edges=g.edges}
  


let translate_cfg (g: miniImp_cfg) (input, output) : miniRisc_cfg =
  let reg_table = Register.new_table 10 in

  (* add input/output variables to register table*)
  add input 0 reg_table;
  add output 1 reg_table;
  
  (* get nodes in order => adding a node is a cons, so nodes are in reverse adding order *)
  let mapped_nodes = List.rev g.nodes
   |> List.map (map_node reg_table)

  in fix_cjumps {nodes=mapped_nodes; edges=g.edges}

