
let () = 
  let p = 
    Lib.MiniImp.Seq(
      Lib.MiniImp.Seq(
        Lib.MiniImp.Assign("x", Lib.MiniImp.Aval(2)),
        Lib.MiniImp.Assign("y", Lib.MiniImp.Aval(1))
      ),
      Lib.MiniImp.If(
        Lib.MiniImp.Minor(Lib.MiniImp.Var("y"), Lib.MiniImp.Aval(0)),
        Lib.MiniImp.Seq(
          Lib.MiniImp.Assign( "y", Lib.MiniImp.Plus(Lib.MiniImp.Var("x"), Lib.MiniImp.Aval(3))),
          Lib.MiniImp.Assign("x", Lib.MiniImp.Var("y"))
        ),
        Lib.MiniImp.Seq(
          Lib.MiniImp.Assign("y", Lib.MiniImp.Aval(10000)),
          Lib.MiniImp.Assign( "x", Lib.MiniImp.Minus(Lib.MiniImp.Aval(1), Lib.MiniImp.Var("y")))
        )
      )
    )
  in
  let g = Lib.Generate.program_to_cfg p in
  let g = Lib.Translate_cfg.translate_cfg g in
  Lib.MiniRisc_cfg.pp_cfg g
  
  

  (*
  let p =
    Lib.MiniImp.Seq(
      Lib.MiniImp.Assign("x", Lib.MiniImp.Aval(0)),
      Lib.MiniImp.While(
        Lib.MiniImp.Minor(Var("x"), Aval(10)),
        Lib.MiniImp.Assign("x", Lib.MiniImp.Plus(Lib.MiniImp.Var("x"), Lib.MiniImp.Aval(1)))
      )
    )
  in
  let code = Lib.Translate.stmt_translate (Lib.Register.new_table 10) p
  in print_string (Lib.Translate.get_code_string code)
  *)