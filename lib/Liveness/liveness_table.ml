module RegisterSet = Register_set.RegisterSet

let show_set (s: Register_set.t) : string =
  String.concat ", " @@ List.map Register.string_of_register @@ RegisterSet.elements s 

type liveness = {
  live_in: Register_set.t;
  live_out: Register_set.t
} [@@ deriving show]


let show_liveness (liveness: liveness): string = 
  let in_str =  show_set liveness.live_in in
  let out_str = show_set liveness.live_out in
  "\tlive_in: " ^ in_str ^ "\n\tlive_out: " ^ out_str 


type liveness_table = (Param_cfg.label, liveness) Hashtbl.t [@@ deriving show]


let show_liveness_table (table: liveness_table) : string =
  Hashtbl.fold (fun label liveness acc ->
    let label_str = string_of_int label in
    let liveness_str = show_liveness liveness in
    acc ^ label_str ^ " ->\n" ^ liveness_str ^ "\n"
  ) table ""

let equal_liveness_table lt1 lt2 =
  Hashtbl.fold (fun label liveness acc ->
    acc && (Hashtbl.find_opt lt2 label = Some liveness)
  ) lt1 true
  && Hashtbl.fold (fun label liveness acc ->
    acc && (Hashtbl.find_opt lt1 label = Some liveness)
  ) lt2 true