(*
keep a table of currently used register and a memory. 
let's assume that after every operation, the result register is immediately stored in memory; 
so before every instruction the used register are loaded from memory. 

we have 3 reserved register: 
r0 is for the input; r1 is for the output, r2 is the temporary register (ie we load here the address of memory in which store/load).
*)

open MiniRisc

type address = int
(* Generate a new memory address *)
let get_new_address =
  let counter = ref 0 in
  fun () ->
    let addr = !counter in
    incr counter;
    addr


type spilled_register = (Register.register, address) Hashtbl.t 

(* Get the memory address of a spilled register *)
let get_register_address (spilled_regs: spilled_register) (reg: Register.register) : address =
  try
    Hashtbl.find spilled_regs reg
  with Not_found ->
    let addr = get_new_address () in
    Hashtbl.add spilled_regs reg addr;
    addr


let transf_instr   
  (ct: Interference_graph.color_table) 
  (spilled_regs: Interference_graph.spill_table) 
  (instr: MiniRisc.exp) 
  : MiniRisc.exp list =

  (* handle freeing/occupying of temp registers*)
  let temp_regs = [-1; -2] in

  (* Mutable set to track in-use registers *)
  let in_use = Hashtbl.create (List.length temp_regs) in

  (* Allocate a free temporary register *)
  let allocate_temp_register () =
    try
      let reg = List.find (fun r -> not (Hashtbl.mem in_use r)) temp_regs in
      Hashtbl.add in_use reg true;
      reg
    with 
      Not_found -> failwith "No available temporary registers!"
  in
  (* Free a temporary register *)
  let free_temp_register reg =
    if Hashtbl.mem in_use reg then
      Hashtbl.remove in_use reg
    else
      failwith ("Register " ^ Register.string_of_register reg ^ " is not allocated!")
  in

  let load_register reg =
    match Hashtbl.find_opt ct reg with
    | Some(phys_reg) -> [], phys_reg
    | None ->
        let addr = get_register_address spilled_regs reg in
        let rtemp = allocate_temp_register () in
        [LoadI(addr, rtemp); Load(rtemp, rtemp)], rtemp
  in

  let spill_register (reg: Register.register): MiniRisc.exp list * Register.register =
    match Hashtbl.find_opt ct reg with
    | Some(phys_reg) -> [], phys_reg
    | None ->
      let addr = get_register_address spilled_regs reg in
      let rtemp = allocate_temp_register () in 
      free_temp_register rtemp;
      [LoadI(addr, rtemp); Store(reg, rtemp)], rtemp
  in

  match instr with
  | AddR(r1, r2, r3) | SubR(r1, r2, r3) | MultR(r1, r2, r3) 
  | AndR(r1, r2, r3) | LessR(r1, r2, r3) ->
    let load_r1, r1_temp = load_register r1 in
    let load_r2, r2_temp = load_register r2 in

    let r3_temp = allocate_temp_register () in
    let addr = get_register_address spilled_regs r3 in
    let r_spill = r2_temp in

    load_r1 @ load_r2 @ [
      (match instr with
      | AddR(_, _, _) -> AddR(r1_temp, r2_temp, r3_temp)
      | SubR(_, _, _) -> SubR(r1_temp, r2_temp, r3_temp)
      | MultR(_, _, _) -> MultR(r1_temp, r2_temp, r3_temp)
      | AndR(_, _, _) -> AndR(r1_temp, r2_temp, r3_temp)
      | LessR(_, _, _) -> LessR(r1_temp, r2_temp, r3_temp)
      | _ -> failwith "Invalid instruction")
    ] @ [LoadI(addr, r_spill); Store(r3_temp, r_spill)]

  | AddI(r1, _, r2) | SubI(r1, _, r2) 
  | MultI(r1, _, r2) | AndI(r1, _, r2) ->
    let load_r1, r1_temp = load_register r1 in
    let spill_r2, r2_temp = spill_register r2 in

    load_r1 @ [
      (match instr with 
      | AddI(_, n, _) -> AddI(r1_temp, n, r2_temp)
      | SubI(_, n, _) -> SubI(r1_temp, n, r2_temp)
      | MultI(_, n, _) -> MultI(r1_temp, n, r2_temp)
      | AndI(_, n, _) -> AndI(r1_temp, n, r2_temp)
      | _ -> failwith "Invalid instruction")
    ] @ spill_r2

  | NotR(r1, r2) | CopyR(r1, r2) ->
    let load_r1, r1_temp = load_register r1 in
    let spill_r2, r2_temp = spill_register r2 in

    load_r1 @ [
      (match instr with
      | NotR(_, _) -> NotR(r1_temp, r2_temp)
      | CopyR(_, _) -> CopyR(r1_temp, r2_temp)
      | _ -> failwith "Invalid instruction")
    ] @ spill_r2
  
  | Load(r1, r2) ->
    let load_r1, r1_temp = load_register r1 in
    let spill_r2, r2_temp = spill_register r2 in

    load_r1 @ [Load(r1_temp, r2_temp)] @ spill_r2
  

  | LoadI(n, r1) ->
    (match Hashtbl.find_opt ct r1 with
    | Some phys_r -> [LoadI(n, phys_r)] (* If `r1` is a physical register, use it directly *)
    | None ->
        let rtemp = allocate_temp_register () in
        let addr = get_register_address spilled_regs r1 in
        
        let r_spill = allocate_temp_register () in
        let spill_code = [LoadI(addr, r_spill); Store(rtemp, r_spill)] in
        free_temp_register rtemp; (* Free the temporary register after use *)
        [LoadI(n, rtemp)] @ spill_code
    )
  
  | CJump(r, l1, l2) ->
    let load_r, r_temp = load_register r in
    load_r @ [CJump(r_temp, l1, l2)]

  | Store (_, _) |Jump(_) | Nop 
  | Int(_) |Bool(_) |Label(_) -> [instr]


type node = MiniRisc_cfg.miniRisc_instr Param_cfg.node

let reg_allocation 
(g: MiniRisc_cfg.miniRisc_cfg)
(ct: Interference_graph.color_table) 
(st: Interference_graph.spill_table) 
: MiniRisc_cfg.miniRisc_cfg =
    let nodes = g.nodes in
    let mapped_nodes = List.map (fun ({id=id; content=c}: node) ->
      let updated_content = List.concat @@ List.map (fun instr -> transf_instr ct st instr) c
      in ({id=id; content=updated_content}: node)
    ) nodes
    in {nodes=mapped_nodes; edges=g.edges}