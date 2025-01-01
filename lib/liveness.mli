module RegisterSet : Set.S with type elt = Register.register

type def_use = {
  def: RegisterSet.t;
  use: RegisterSet.t
} [@@ deriving show]


type liveness = {
  live_in: RegisterSet.t;
  live_out: RegisterSet.t
} [@@ deriving show]


val compute_use_def_table: MiniRisc_cfg.miniRisc_cfg -> (Param_cfg.label, def_use) Hashtbl.t 
val show_use_def_table: (Param_cfg.label, def_use) Hashtbl.t -> unit


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t [@@ deriving show]
val liveness_analysis: MiniRisc_cfg.miniRisc_cfg -> liveness_table
val show_liveness_table: liveness_table -> string
