open MiniRisc

type node = MiniRisc_cfg.miniRisc_instr Param_cfg.node

(* Transform a CFG *)
let reg_allocation 
  (g: MiniRisc_cfg.miniRisc_cfg)
  (k: int)
  (current_state: Register_state.reg_state)
  : MiniRisc_cfg.miniRisc_cfg =

  (* exclude r-2 => used for storing addresses*)
  let temp_regs = List.init (k-1) (fun x -> x - 1) in
  let in_use: Register.register list ref = ref [] in


  let allocate_temp_register (current_state: Register_state.reg_state) : MiniRisc.exp list * Register.register =
    try
      Printf.printf "used: ";
      List.iter (fun r -> Printf.printf "%s, " (Register.string_of_register r)) !in_use;
      
      let reg = List.find (fun r -> not (List.mem r !in_use)) temp_regs in
      print_endline ("\nFree to use: " ^ Register.string_of_register reg);
      (*
      Hashtbl.replace current_state reg (`Physical(reg));
      *)
      
      (* add reg to the used registers; the idea is that LR Used regs are at the beginning *)
      in_use := !in_use @ [reg];
      [], reg

    with Not_found -> 
      print_endline "\nNo free register";
          
      (*physical reg to spill*)
      let to_spill = List.hd !in_use in
      in_use := List.filter (fun r -> r <> to_spill) !in_use;
      
      (* get address of vreg associated to phys_reg chosen to spill*)
      let vreg = Register_state.find_vreg_by_location current_state (`Physical(to_spill)) in
      let address = Register_state.get_register_address current_state vreg in
      
      
      let previous_vreg = Register_state.find_vreg_by_location current_state (`Physical(to_spill)) in
      Register_state.update_register_state current_state previous_vreg (`Memory(address));
      
      (*Hashtbl.remove current_state to_spill;*)
      Printf.printf "\nrn: vr%d -> r%d" to_spill previous_vreg;
      Printf.printf "\nsave: r%d -> 0x%d" previous_vreg address;
      Printf.printf "\nSpilling %s to address %d\n" (Register.string_of_register to_spill) address;
      in_use := !in_use @ [to_spill]; (* spilled_register is adde back as the last used*)
      [LoadI(address, -2); Store(to_spill, -2)], to_spill
  in

  (* Transform an instruction *)
  let transf_instr (current_state: Register_state.reg_state) (instr: MiniRisc.exp) : MiniRisc.exp list =

    (* Load a register value if needed *)
    let load_register (current_state: Register_state.reg_state) vreg =
      match Hashtbl.find_opt current_state vreg with
      | Some(`Physical(phys_reg)) -> 
        Register_state.update_register_state current_state vreg (`Physical(phys_reg)); (* probalby is redundant, but here for consistency*)
        [], phys_reg
      | Some(`Memory(addr)) ->
        let spill_code, phys_reg = allocate_temp_register current_state in
        (*
        in_use := !in_use @ [phys_reg];
        *)

        Printf.printf "Loading v%s in %s\n" (Register.string_of_register vreg) (Register.string_of_register phys_reg);
        Register_state.update_register_state current_state vreg (`Physical(phys_reg));
        spill_code @ [LoadI(addr, phys_reg); Load(phys_reg, phys_reg)], phys_reg
      | None ->
          failwith ("Register " ^ Register.string_of_register vreg ^ " is uninitialized!")
    in

    Register_state.show_reg_state current_state;
    string_of_risc_exp instr |> print_endline;

    (*
    Printf.printf "used registers before %s" (MiniRisc.string_of_risc_exp instr);
    List.iter (fun r -> print_endline ("\t" ^ Register.string_of_register r) ) !in_use;
    *)


    match instr with
    | AddR(r1, r2, r3) | SubR(r1, r2, r3) | MultR(r1, r2, r3) 
    | AndR(r1, r2, r3) | LessR(r1, r2, r3) ->
      let load_r1, r1_temp = load_register current_state r1 in
      print_endline @@ (Register.string_of_register r1) ^ "->" ^ (Register.string_of_register r1_temp);

      let load_r2, r2_temp = load_register current_state r2 in
      print_endline @@ (Register.string_of_register r2) ^ "->" ^ (Register.string_of_register r2_temp);


      let spill_r3, r3_temp = allocate_temp_register current_state
      (*optmize: if two regs are the same
        if r1 <> r3 
          then allocate_temp_register current_state instr 
        else [], r1_temp
        *)
      in Register_state.update_register_state current_state r3 (`Physical(r3_temp));
    
      load_r1 @ load_r2 @ spill_r3 @ [
        (match instr with
        | AddR(_, _, _) -> AddR(r1_temp, r2_temp, r3_temp)
        | SubR(_, _, _) -> SubR(r1_temp, r2_temp, r3_temp)
        | MultR(_, _, _) -> MultR(r1_temp, r2_temp, r3_temp)
        | AndR(_, _, _) -> AndR(r1_temp, r2_temp, r3_temp)
        | LessR(_, _, _) -> LessR(r1_temp, r2_temp, r3_temp)
        | _ -> failwith "Invalid instruction")
      ]

    | AddI(r1, _, r2)  | SubI(r1, _, r2) 
    | MultI(r1, _, r2) | AndI(r1, _, r2) ->
      let load_r1, r1_temp = load_register current_state r1 in
      let spill_r2, r2_temp = allocate_temp_register current_state in
      Register_state.update_register_state current_state r2 (`Physical(r2_temp));

      load_r1 @ spill_r2 @ [
        (match instr with 
        | AddI(_, n, _) -> AddI(r1_temp, n, r2_temp)
        | SubI(_, n, _) -> SubI(r1_temp, n, r2_temp)
        | MultI(_, n, _) -> MultI(r1_temp, n, r2_temp)
        | AndI(_, n, _) -> AndI(r1_temp, n, r2_temp)
        | _ -> failwith "Invalid instruction")
      ]

    | NotR(r1, r2) | CopyR(r1, r2) ->
      let load_r1, r1_temp = load_register current_state r1 in
      let spill_r2, r2_temp = allocate_temp_register current_state in
      Register_state.update_register_state current_state r2 (`Physical(r2_temp));

      load_r1 @ spill_r2 @ [
        (match instr with
        | NotR(_, _) -> NotR(r1_temp, r2_temp)
        | CopyR(_, _) -> CopyR(r1_temp, r2_temp)
        | _ -> failwith "Invalid instruction")
      ]

    | Load(r1, r2) ->
      let load_r1, r1_temp = load_register current_state r1 in
      let spill_r2, r2_temp = allocate_temp_register current_state in
      Register_state.update_register_state current_state r2 (`Physical(r2_temp));

      load_r1 @ spill_r2 @ [Load(r1_temp, r2_temp)]

    | LoadI(n, r1) ->
      let spill_r1, r1_temp = allocate_temp_register current_state in
      Register_state.update_register_state current_state r1 (`Physical(r1_temp));

      (* correct? 
      in_use := !in_use @ [r1_temp];
      *)
      
      spill_r1 @ [LoadI(n, r1_temp)]

    | CJump(r, l1, l2) ->
      let load_r, r_temp = load_register current_state r in
      load_r @ [CJump(r_temp, l1, l2)]

    | Store(r1, r2) ->
      let load_r1, r1_temp = load_register current_state r1 in
      let load_r2, r2_temp = load_register current_state r2 in
      load_r1 @ load_r2 @ [Store(r1_temp, r2_temp)]

    | Jump(_) | Nop | Int(_) | Bool(_) | Label(_) -> [instr]
  in


  (* we can assume r0 is always used at the beginning *)
  in_use := !in_use @ [0];

  let nodes = g.nodes in
  let mapped_nodes = List.map (fun ({id=id; content=c}: node) ->
    let updated_content = List.concat @@ List.map (fun instr -> transf_instr current_state instr) c 
    in ({id=id; content=updated_content} : node)
  ) nodes in
  {nodes=mapped_nodes; edges=g.edges}
