open Param_cfg

type miniImp_instr = 
  | Statement of MiniImp.stmt
  | Expression of MiniImp.exp 


type miniImp_cfg = miniImp_instr Param_cfg.cfg

let create_node content = 
  let id = fresh_id () in
  { id; content }