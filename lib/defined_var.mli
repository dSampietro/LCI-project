type def_var_table
val show_def_var_table: def_var_table -> unit

(* forward analysis: start from label 0 *)
(* MiniImp cfg is minimal: instruction is one of assignement or use*)
val defined_vars_analysis: MiniImp_cfg.miniImp_cfg -> string -> unit
