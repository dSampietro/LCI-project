type liverange
type liverange_table

val compute_live_ranges: Liveness.liveness_table -> liverange_table
val show_liverange_table: liverange_table -> unit

type interf_graph
val build_interf_graph: liverange_table -> interf_graph 
val show_intf_graph: interf_graph -> unit
