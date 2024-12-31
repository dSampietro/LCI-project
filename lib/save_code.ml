type node = MiniRisc_cfg.miniRisc_instr Param_cfg.node


let codegen (g: MiniRisc_cfg.miniRisc_cfg): string list =
  let compare_nodes_by_id (node1: node) (node2: node) =
    compare node1.id node2.id
  in
  let nodes = List.sort compare_nodes_by_id g.nodes in
  List.map (fun ({id=id; content=c}: MiniRisc_cfg.miniRisc_instr Param_cfg.node) ->
    let label = MiniRisc.Label(string_of_int id) in
    if List.exists (function MiniRisc.CJump(_, _, _) -> true | _ -> false) c then
      Translate.get_code_string (label::c)
    else
      match Param_cfg.successors g id with
      | x::_ -> 
        let succ_id = string_of_int x in
        let jump_instr = if x != id+1 then [MiniRisc.Jump(succ_id)] else [] in
        Translate.get_code_string (label :: c @ jump_instr)
      | _ -> Translate.get_code_string (label::c)
  ) nodes
