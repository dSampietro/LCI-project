module RegisterSet : Set.S with type elt = int

type def_use = {
  def: RegisterSet.t;
  use: RegisterSet.t
} [@@ deriving show]


type liveness = {
  live_in: RegisterSet.t;
  live_out: RegisterSet.t
} [@@ deriving show]


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t [@@ deriving show]
val liveness_analysis: MiniRisc_cfg.miniRisc_cfg -> liveness_table
val show_liveness_table: liveness_table -> string
