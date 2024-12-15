open Register

let string_of_risc_exp (e: MiniRisc.exp) = 
  match e with
  | Nop -> "\tNop\n"

  (*urop*)
  | NotR(r1, r2) ->  Printf.sprintf "\tNotR %s %s\n" (string_of_register r1) (string_of_register r2)
  | CopyR(r1, r2) -> Printf.sprintf "\tCopyR %s %s\n" (string_of_register r1) (string_of_register r2)

  (*brop*)
  | AddR(r1, r2, r3) ->  Printf.sprintf "\tAddR %s %s %s\n" (string_of_register r1) (string_of_register r2) (string_of_register r3)
  | SubR(r1, r2, r3) ->  Printf.sprintf "\tSubR %s %s %s\n" (string_of_register r1) (string_of_register r2) (string_of_register r3)
  | MultR(r1, r2, r3) -> Printf.sprintf "\tMultR %s %s %s\n" (string_of_register r1) (string_of_register r2) (string_of_register r3)
  | AndR(r1, r2, r3) ->  Printf.sprintf "\tAndR %s %s %s\n" (string_of_register r1) (string_of_register r2) (string_of_register r3)
  | LessR(r1, r2, r3) -> Printf.sprintf "\tLessR %s %s %s\n" (string_of_register r1) (string_of_register r2) (string_of_register r3)

  (*biop*)
  | AddI(r1, n, r2) ->  Printf.sprintf "\tAddI %s %d %s\n" (string_of_register r1) n (string_of_register r2)
  | SubI(r1, n, r2) ->  Printf.sprintf "\tSubI %s %d %s\n" (string_of_register r1) n (string_of_register r2)
  | MultI(r1, n, r2) -> Printf.sprintf "\tMultI %s %d %s\n" (string_of_register r1) n (string_of_register r2)
  | AndI(r1, n, r2) ->  Printf.sprintf "\tAndI %s %d %s\n" (string_of_register r1) n (string_of_register r2)

  (*load/store*)
  | Load(r1, r2) -> Printf.sprintf "\tLoad %s %s\n" (string_of_register r1) (string_of_register r2)
  | LoadI(n, r2) -> Printf.sprintf "\tLoadI %d %s\n" n (string_of_register r2)
  | Store(r1, r2) -> Printf.sprintf "\tStore %s %s\n" (string_of_register r1) (string_of_register r2)

  (*jump*)
  | Jump(l) -> "\tJump " ^ l ^ "\n"
  | CJump(r1, l1, l2) -> Printf.sprintf "\tCJump %s %s %s\n" (string_of_register r1) l1 l2

  (* only for represent (int/bool) values*)
  | Int(n) -> string_of_int n
  | Bool(b) -> string_of_bool b
  | Label(s) -> s ^ ": \n"