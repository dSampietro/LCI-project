let map_reg_to_color (ct: Interference_graph.color_table) (r: Register.register) : Interference_graph.color =
  match Hashtbl.find_opt ct r with
  | Some(c) -> c
  | None -> r


let map_instruction (ct: Interference_graph.color_table) (instr: MiniRisc.exp) : MiniRisc.exp = 
  match instr with
    (*urop*)
    | NotR(r1, r2)  ->  NotR(map_reg_to_color ct r1, map_reg_to_color ct r2)
    | CopyR(r1, r2) -> CopyR(map_reg_to_color ct r1, map_reg_to_color ct r2)
    
    (*brop*)
    | AddR(r1, r2, r3)  ->  AddR(map_reg_to_color ct r1, map_reg_to_color ct r2, map_reg_to_color ct r3)
    | SubR(r1, r2, r3)  ->  SubR(map_reg_to_color ct r1, map_reg_to_color ct r2, map_reg_to_color ct r3)
    | MultR(r1, r2, r3) -> MultR(map_reg_to_color ct r1, map_reg_to_color ct r2, map_reg_to_color ct r3)
    | AndR(r1, r2, r3)  ->  AndR(map_reg_to_color ct r1, map_reg_to_color ct r2, map_reg_to_color ct r3) 
    | LessR(r1, r2, r3) -> LessR(map_reg_to_color ct r1, map_reg_to_color ct r2, map_reg_to_color ct r3)
    
    (*biop*)
    | AddI(r1, n, r2)  ->  AddI(map_reg_to_color ct r1, n, map_reg_to_color ct r2)  
    | SubI(r1, n, r2)  ->  SubI(map_reg_to_color ct r1, n, map_reg_to_color ct r2)  
    | MultI(r1, n, r2) -> MultI(map_reg_to_color ct r1, n, map_reg_to_color ct r2) 
    | AndI(r1, n, r2)  ->  AndI(map_reg_to_color ct r1, n, map_reg_to_color ct r2)  
    
    (*load/store*)
    | Load(r1, r2)  -> Load(map_reg_to_color ct r1, map_reg_to_color ct r2)
    | LoadI(n, r2)  -> LoadI(n, map_reg_to_color ct r2)
    | Store(r1, r2) -> Store(map_reg_to_color ct r1, map_reg_to_color ct r2)
  
    (*jump*)
    | CJump(r1, l1, l2) -> CJump(map_reg_to_color ct r1, l1, l2)
  
    | _ -> instr


type node = MiniRisc_cfg.miniRisc_instr Param_cfg.node
let reg_allocation (g: MiniRisc_cfg.miniRisc_cfg) (ct: Interference_graph.color_table) (_st: Interference_graph.spill_table) : MiniRisc_cfg.miniRisc_cfg =
  let nodes = g.nodes in
  let mapped_nodes = List.map (fun ({id=id; content=c}: node) ->
    let updated_content = List.map (fun instr -> map_instruction ct instr) c
    in ({id=id; content=updated_content}: node)
  ) nodes
  in {nodes=mapped_nodes; edges=g.edges}
