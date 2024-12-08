open CFG


val string_of_exp: MiniImp.exp -> string
val string_of_stmt: MiniImp.stmt -> string 
val string_of_node: node -> string

(* Pretty-print the CFG *)
val pp_cfg: CFG.t -> unit