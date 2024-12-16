type miniRisc_instr = MiniRisc.exp list 
type miniRisc_cfg = miniRisc_instr Param_cfg.cfg

val create_node: miniRisc_instr ->  miniRisc_instr Param_cfg.node

val pp_cfg: miniRisc_cfg -> unit
