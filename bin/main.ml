let parse (filename: string) : Lib.MiniImp.program =
  let input_file = open_in filename in
  let lexbuf = Lexing.from_channel input_file in

  try
    let program = (Lib.MyParser.prog Lib.MyLexer.read lexbuf) in
    close_in input_file;
    program
  with
  | Lib.MyParser.Error ->
    close_in input_file;
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "Parse error at line %d, column %d: unexpected token\n"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1);
    failwith "Parse error"

  | Parsing.Parse_error ->
      close_in input_file;
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1);
      failwith "Parsing.Parse_error"

  | exn -> 
    close_in input_file; 
    Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
    failwith (Printexc.to_string exn)


(* usage: 
  ./cc 
  --init_check
  --optimize
  -r <num_reg> 
  -o <output_file> 
  <input_file>     
*)

let usage_msg = "Usage: compiler [-init_check] [-optimize] [-r <num_reg>] [-o <output_file>] <input_file>"

let init_check = ref false
let optimize = ref false
let num_reg = ref 4 
let output_name = ref "a"
let input_file = ref ""

let anon_fun filename = 
  input_file := filename

let options = [
  ("--init_check", Arg.Set init_check, "Enable init check");
  ("--optimize", Arg.Set optimize, "Enable optimization");
  ("-r", Arg.Set_int num_reg, "Number of registers");
  ("-o", Arg.Set_string output_name, "Output file");
]


let () = 
  Arg.parse options anon_fun usage_msg;

    let Main(inp, out, p) = parse !input_file in
    let g = Lib.Generate.program_to_cfg p in
    let g = Lib.Translate_cfg.translate_cfg g (inp, out) in
    Lib.MiniRisc_cfg.pp_cfg g;


    let g1 = if !optimize then
      (* Liveness - live ranges *)
      let _udt = Lib.Liveness.compute_use_def_table g in
      let lt = Lib.Liveness.liveness_analysis g in
      let lrt = Lib.Interference_graph.compute_live_ranges lt in
 
      (* Interference graph *)
      let int_g = Lib.Interference_graph.build_interf_graph lrt in
    
      (* k-coloring*)
      let current_state = Lib.Interference_graph.kcoloring int_g (!num_reg-2)
      in 
      (*
      Lib.Interference_graph.show_color_table ct;
      Lib.Interference_graph.show_spill_table st;
      *)

      (* reg allocation *)
      (*Lib.Allocation_v3.reg_allocation g ct st*)
      Lib.Allocation_v4.reg_allocation g current_state
      (*
      let g1 = Lib.Allocation_v3.reg_allocation g ct st
      in Lib.MiniRisc_cfg.pp_cfg g1;
      *)
    else
      g

    in
    let final_code = Lib.Save_code.codegen g1 in
    List.iter (fun x -> print_endline x) final_code;

    let output_file = open_out (!output_name ^ ".minirisc") in
    List.iter (fun x -> output_string output_file (x ^ "\n")) final_code;
    close_out output_file