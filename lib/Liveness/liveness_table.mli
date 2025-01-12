
type liveness = {
  live_in: Register_set.t;
  live_out: Register_set.t
} [@@ deriving show]


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t [@@ deriving show]
val show_liveness_table: liveness_table -> string
val equal_liveness_table: liveness_table -> liveness_table -> bool
