type address = int

(* Generate a new memory address *)
let get_new_address =
  let counter = ref 100 in
  fun () ->
    let addr = !counter in
    incr counter;
    addr

(* A map to keep track of current register states *)
(* used to combine info given by color_table & spill_table *)
type location = [`Physical of Register.register | `Memory of address]
let string_of_location loc = 
  match loc with
  | `Physical(r) -> Register.string_of_register r
  | `Memory(addr) -> "M(" ^ string_of_int addr ^ ")"

type reg_state = (Register.register, location) Hashtbl.t

(* Update register states after an operation *)
let update_register_state (current_state: reg_state) target_reg phys_loc =
  Printf.printf "current_state update\tv%s: %s\n" (Register.string_of_register target_reg) (string_of_location phys_loc);
  Hashtbl.replace current_state target_reg phys_loc


let find_vreg_by_location (current_state: reg_state) (value: location) =
  Printf.printf "Finding key with value %s\n" (string_of_location value);
  let found = ref None in
  Hashtbl.iter (fun key v ->
    if v = value then found := Some key
  ) current_state;
  match !found with
  | Some(key) -> key
  | None -> 
    match value with
    | `Physical(reg) -> reg
    | `Memory(_) -> failwith "No register found for the given memory address"

    (*
    print_endline @@ "No key is mapped to " ^ (string_of_location value); 
    raise Not_found
    *)

let show_reg_state (rs: reg_state) =
  Hashtbl.iter (fun reg state ->
    Printf.printf "%s -> " (Register.string_of_register reg);
    match state with
    | `Physical(phys_reg) -> Printf.printf "Physical(%s)\n" (Register.string_of_register phys_reg)
    | `Memory(addr) -> Printf.printf "Memory(%d)\n" addr
  ) rs;
  print_endline "\n"



let get_register_address (reg_state: reg_state) (reg: Register.register) : address =
  match Hashtbl.find_opt reg_state reg with
  | Some(`Memory(addr)) -> addr
  | Some(`Physical(_)) -> get_new_address ()
  | None -> get_new_address ()
  (*
  failwith ("Register " ^ Register.string_of_register reg ^ " do not found")
  *)