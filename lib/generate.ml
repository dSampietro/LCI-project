open MiniImp
open CFG


let rec generate_cfg (cfg: CFG.t) (stmt: MiniImp.stmt) (id: CFG.label): (CFG.t * CFG.label) = 
  match stmt with
  | Skip | Assign(_, _) ->
    let (id', node_id) = CFG.fresh_id id in
    let node = CFG.Statement(node_id, stmt) in
    let updated_cfg = CFG.add_node cfg node in
    (updated_cfg, id') (* return CFG, last_node_id*)
  
  | Seq(s1, s2) ->
    let (cfg1, id1) = generate_cfg cfg s1 id in
    let s1_last_node_id = CFG.get_node_id (List.hd (get_nodes cfg1)) in (* id1?*)
    
    let (cfg2, id2) = generate_cfg cfg1 s2 id1 in
    let (cfg2_dummy, _) = generate_cfg (CFG.empty()) s2 id1 in
    let s2_first_node_id = CFG.get_first_node_id cfg2_dummy in
    
    print_endline("s1_last_node_id: " ^ string_of_int(s1_last_node_id));
    print_endline("s2_first_node_id: " ^ string_of_int(s2_first_node_id));

    let updated_cfg = CFG.add_edge cfg2 s1_last_node_id s2_first_node_id in
    (updated_cfg, id2)
  
  | If(cond, then_branch, else_branch) ->
    let (id1, node_id) = CFG.fresh_id id in
    let cond_node = CFG.Expression(node_id, cond) in
    let cfg = CFG.add_node cfg cond_node in
    
    (* cfg with then, else nodes added*)
    let (cfg_then, id2) = generate_cfg cfg then_branch id1 in
    let then_start_id = id1 in (*CFG.get_first_node_id cfg_then in*)
    let then_end_id = CFG.get_last_node_id cfg_then in

    let (cfg_else, id3) = generate_cfg cfg_then else_branch id2 in
    let else_start_id = id2 in (*CFG.get_first_node_id cfg_else in*)
    let else_end_id = CFG.get_last_node_id cfg_else in

    (* connecting cond with then/else start *)
    let cfg = CFG.add_edge cfg_else node_id then_start_id in    (* cond -> then start *)
    let cfg = CFG.add_edge cfg      node_id else_start_id in    (* cond -> else_start *)


    let (_, skip_node_id) = CFG.fresh_id id3 in
    let skip_node = CFG.Statement (skip_node_id, Skip) in
    let cfg = CFG.add_node cfg skip_node in (* add Skip node*)

    let cfg = CFG.add_edge cfg then_end_id skip_node_id in    (* then_end -> skip *)
    let cfg = CFG.add_edge cfg else_end_id skip_node_id in    (* else_end -> skip*)
    (cfg, id2)

  | While(cond, body) ->
    (* add first Skip node *)
    let (id_skip1, skip1_node_id) = CFG.fresh_id id in
    let skip1_node = CFG.Statement(skip1_node_id, Skip) in 
    let cfg = CFG.add_node cfg skip1_node in  (* add skip1 node*)

    (* add condition node *)
    let (id_cond, cond_node_id) = CFG.fresh_id id_skip1 in
    let cond_node = CFG.Expression (cond_node_id, cond) in 
    let cfg = CFG.add_node cfg cond_node in   (* add cond node*)

    let cfg = CFG.add_edge cfg skip1_node_id cond_node_id in (* skip1 -> cond*)


    (* generate CFG of the body*)
    let (cfg_body, _) = generate_cfg (CFG.empty()) body id_cond in
    let body_start_id = CFG.get_first_node_id cfg_body in 
    let body_end_id = CFG.get_last_node_id cfg_body in

    let (cfg_body, id_body) = generate_cfg cfg body id_cond in

    (* add edges*)
    let cfg = CFG.add_edge cfg_body cond_node_id body_start_id in  (* cond -> body_start *)
    let cfg = CFG.add_edge cfg body_end_id cond_node_id in    (* body_end -> cond *)


    (* add last Skip node *)
    let (id_skip2, skip2_node_id) = CFG.fresh_id id_body in
    let skip2_node = CFG.Statement(skip2_node_id, Skip) in 
    let cfg = CFG.add_node cfg skip2_node in  (* add skip2 node*)
    
    let cfg = CFG.add_edge cfg cond_node_id skip2_node_id in
    (cfg, id_skip2)



let program_to_cfg (p: MiniImp.stmt) : CFG.t = 
  let (g, _) = Lib.Generate.generate_cfg (Lib.CFG.empty()) p 0 
  in g


