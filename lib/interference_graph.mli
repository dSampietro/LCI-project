type liverange
type liverange_table

val compute_live_ranges: Liveness.liveness_table -> liverange_table
val show_liverange_table: liverange_table -> unit

type interf_graph
val build_interf_graph: liverange_table -> interf_graph 
val show_intf_graph: interf_graph -> unit

val get_degree_table: interf_graph -> (Param_cfg.label, int) Hashtbl.t 
val show_degree_table: (Param_cfg.label, int) Hashtbl.t -> unit



type color = int
type color_table = (Register.register, color) Hashtbl.t
val show_color_table: color_table -> unit

type address = int
type content
type memory = (address, content) Hashtbl.t
type spill_table = (Register.register, address) Hashtbl.t
val show_spill_table: spill_table -> unit 


val kcoloring: interf_graph -> int -> Register_state.reg_state