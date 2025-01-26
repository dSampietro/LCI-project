type miniImp_instr = 
  | Statement of MiniImp.stmt
  | Expression of MiniImp.exp 

type miniImp_cfg = miniImp_instr Param_cfg.cfg

val create_node: miniImp_instr ->  miniImp_instr Param_cfg.node

val pp_cfg: miniImp_cfg -> unit
