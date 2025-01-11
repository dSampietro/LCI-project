open MiniImp
open MiniImp_cfg


(* return CFG, last_node_id*)
let rec generate_cfg (cfg: miniImp_cfg) (stmt: MiniImp.stmt): (miniImp_cfg * Param_cfg.label) =
  match stmt with
  | Skip | Assign(_, _) ->
    let node = create_node (Statement(stmt)) in
    let updated_cfg = Param_cfg.add_node cfg node in
    (updated_cfg, node.id) 
  
  | Seq(s1, s2) ->
    let (cfg, id1) = generate_cfg cfg s1 in
    let (cfg, id2) = generate_cfg cfg s2 in
    
    let cfg = Param_cfg.add_edge cfg id1 (id1+1) in
    (cfg, id2)
  
  | If(cond, then_branch, else_branch) ->
    let cond_node = create_node (Expression(cond)) in
    let cfg = Param_cfg.add_node cfg cond_node in

    (* cfg with then, else nodes added*)
    let (cfg, then_end_id) = generate_cfg cfg then_branch in
    let (cfg, else_end_id) = generate_cfg cfg else_branch in

    let then_start_id = cond_node.id + 1 in
    let else_start_id = then_end_id + 1 in

    (* connecting cond with then/else start *)
    let cfg = Param_cfg.add_edge cfg cond_node.id then_start_id in    (* cond -> then start *)
    let cfg = Param_cfg.add_edge cfg cond_node.id else_start_id in    (* cond -> else_start *)

    let skip_node = create_node (Statement(Skip)) in
    let cfg = Param_cfg.add_node cfg skip_node in (* add Skip node*)

    let cfg = Param_cfg.add_edge cfg then_end_id skip_node.id in    (* then_end -> skip *)
    let cfg = Param_cfg.add_edge cfg else_end_id skip_node.id in    (* else_end -> skip*)
    (cfg, skip_node.id)

  | While(cond, body) ->
    (* add first Skip node *)
    let skip_node_1 = create_node (Statement(Skip)) in 
    let cfg = Param_cfg.add_node cfg skip_node_1 in  (* add skip1 node*)

    (* add condition node *)
    let cond_node = create_node (Expression(cond)) in 
    let cfg = Param_cfg.add_node cfg cond_node in   (* add cond node*)
    let cfg = Param_cfg.add_edge cfg skip_node_1.id cond_node.id in (* skip1 -> cond*)

    (* generate CFG of the body*)
    let (cfg, body_end_id) = generate_cfg cfg body in

    (* add edges*)
    let cfg = Param_cfg.add_edge cfg cond_node.id (cond_node.id + 1) in  (* cond -> body_start *)
    let cfg = Param_cfg.add_edge cfg body_end_id cond_node.id in    (* body_end -> cond *)


    (* add last Skip node *)
    let skip_node_2 = create_node (Statement(Skip)) in 
    let cfg = Param_cfg.add_node cfg skip_node_2 in  (* add skip2 node*)
    
    let cfg = Param_cfg.add_edge cfg cond_node.id skip_node_2.id in
    (cfg, skip_node_2.id)


let program_to_cfg (p: MiniImp.stmt) : miniImp_cfg = 
  let (g, _) = generate_cfg (Param_cfg.empty()) p
  in g