let codegen (g: MiniRisc_cfg.miniRisc_cfg): string list =

  let nodes = List.sort Param_cfg.compare_nodes_by_id g.nodes in
  
  List.map (fun ({id=id; content=c}: MiniRisc_cfg.miniRisc_instr Param_cfg.node) ->
    let label = MiniRisc.Label(string_of_int id) in
    (* Trivial solution: add a Jump between every node and its successor (if no CJump in the node) *)
    (* My solution: 
      recognize the node with CJump -> no jump at the end
      other nodes -> add jump only if successor is not the next node
    *)
    (* node contains a CJump*)
    if List.exists (function MiniRisc.CJump(_, _, _) -> true | _ -> false) c then
      Translate.get_code_string (label::c)
    else
      match Param_cfg.successors g id with
      | x::_ -> 
        let succ_id = string_of_int x in
        (* if next node is not the successor, add a jump instruction; otherwise it is trivial *)
        let jump_instr = if x != id+1 then [MiniRisc.Jump(succ_id)] else [] in
        Translate.get_code_string (label :: c @ jump_instr)
      | _ -> Translate.get_code_string (label::c)
  ) nodes
