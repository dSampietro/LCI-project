val exp_translate: Register.register_table -> MiniImp.exp -> Register.register option -> MiniRisc.exp list * Register.register
val stmt_translate: Register.register_table -> MiniImp.stmt -> MiniRisc.exp list
val get_code_string: MiniRisc.exp list -> string 