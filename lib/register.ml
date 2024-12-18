type register = int
  (*
  | Register of int
  *)

let string_of_register (r: register) : string =  "r" ^ (string_of_int r)
  (*
  match r with 
  | Register(n) -> "r" ^ (string_of_int n)
  *)


(* Report:
  r0 = r_in
  r1 = r_out
  _ = other registers
*)
let register_counter = ref (1) (* first register is 2*)
let get_new_register () : register = 
  incr register_counter;
  (* Register(!register_counter) *)
  !register_counter


type label = string
let label_counter = ref 0
let get_new_label () : label =
  incr label_counter;
  "L" ^ (string_of_int !label_counter)

(* Map(Var -> Register) *)
(* reg_table is an Hashtbl because we need in-place mutability *)
type register_table = (string, register) Hashtbl.t

let new_table (n: int) : register_table = Hashtbl.create n

(* Update the value of a variable in memory *)
let lookup (rt: register_table) (id: string): register option =
  Hashtbl.find_opt rt id 

let lookup_or_add (rt: register_table) (id: string): register =
  match Hashtbl.find_opt rt id with
  | Some(reg) -> reg
  | None -> 
    let reg = get_new_register () in
    Hashtbl.add rt id reg;
    reg

let add (id: string) (reg: register) (rt: register_table) = 
  Hashtbl.add rt id reg 
