type register = int

val string_of_register: register -> string
val get_new_register: unit -> register

type label = string
val get_new_label: unit -> label

type register_table
val new_table: int -> register_table
val lookup: register_table -> string -> register option
val lookup_or_add: register_table -> string -> register
val add: string -> register -> register_table -> unit
