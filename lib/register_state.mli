type address = int

(* Generate a new memory address *)
val get_new_address: unit -> address

(* A map to keep track of current register states
 used to combine info given by color_table & spill_table *)
type location = [`Physical of Register.register | `Memory of address]
val string_of_location: location -> string

type reg_state = (Register.register, location) Hashtbl.t

(* Update register states after an operation *)
val update_register_state: reg_state -> Register.register -> location -> unit
val find_vreg_by_location: reg_state -> location -> Register.register
val show_reg_state: reg_state -> unit
val get_register_address: reg_state -> Register.register -> address
