type def_use = {
  def: Register_set.t;
  use: Register_set.t
} [@@ deriving show]


val compute_use_def_table: MiniRisc_cfg.miniRisc_cfg -> (Param_cfg.label, def_use) Hashtbl.t 
val show_use_def_table: (Param_cfg.label, def_use) Hashtbl.t -> unit