

let () = 
  let p = 
    Lib.MiniImp.Seq(
      Lib.MiniImp.Assign("x", Lib.MiniImp.Aval(2)),
      Lib.MiniImp.If(Lib.MiniImp.Minor(Lib.MiniImp.Var("y"), Lib.MiniImp.Aval(0)),
      Lib.MiniImp.Seq(
          Lib.MiniImp.Assign(
            "y", Lib.MiniImp.Plus(Lib.MiniImp.Var("x"), Lib.MiniImp.Aval(3))),
          Lib.MiniImp.Assign("x", Lib.MiniImp.Var("y"))
        ),
        Lib.MiniImp.Assign(
          "x", Lib.MiniImp.Minus(Lib.MiniImp.Aval(1), Lib.MiniImp.Var("y"))
        )
      )
    )

  in 
  let (g, _) = Lib.Generate.generate_cfg (Lib.CFG.empty()) p 0 
  in Lib.CFG_pp.pp_cfg g
(*
in Printf.printf "#nodes: %d\n" (List.length (Lib.CFG.get_nodes g))
*)
