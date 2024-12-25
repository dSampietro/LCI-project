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


let () = 

  if Array.length Sys.argv != 2 then
    Printf.eprintf "Argument MiniImp-program is needed"
  else
    let filename = Sys.argv.(1) in
    let Main(inp, out, p) = parse filename in
    let g = Lib.Generate.program_to_cfg p in
    let g = Lib.Translate_cfg.translate_cfg g (inp, out) in
    Lib.MiniRisc_cfg.pp_cfg g;
    let udt = Lib.Liveness.compute_use_def_table g in
    print_endline "Use-def table:\n"; 
    Lib.Liveness.show_use_def_table udt;
    
    let lt = Lib.Liveness.liveness_analysis g in
    print_endline ("\nLiveness table\n" ^ Lib.Liveness.show_liveness_table lt);
    let lrt = Lib.Interference_graph.compute_live_ranges lt in
    print_endline "\n\nLive ranges:\n";
    Lib.Interference_graph.show_liverange_table lrt;
    let int_g = Lib.Interference_graph.build_interf_graph lrt in
    Lib.Interference_graph.show_intf_graph int_g;