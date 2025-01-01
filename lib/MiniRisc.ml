open Register

type reg = Register.register
type label = string

type exp = 
  | Nop

  (*urop*)
  | NotR of reg * reg
  | CopyR of reg * reg

  (*brop*)
  | AddR of reg * reg * reg
  | SubR of reg * reg * reg
  | MultR of reg * reg * reg
  | AndR of reg * reg * reg
  | LessR of reg * reg * reg

  (*biop*)
  | AddI of reg * int * reg
  | SubI of reg * int * reg
  | MultI of reg * int * reg
  | AndI of reg * int * reg

  (*load/store*)
  | Load of reg * reg
  | LoadI of int * reg
  | Store of reg * reg

  (*jump*)
  | Jump of label
  | CJump of reg * label * label

  (* only for represent (int/bool )values*)
  | Int of int
  | Bool of bool
  | Label of string


(* for pretty-printing*)

let string_of_risc_exp (e: exp) = 
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
  | Jump(l) -> Printf.sprintf "\tJump %s\n" l
  | CJump(r1, l1, l2) -> Printf.sprintf "\tCJump %s %s %s\n" (string_of_register r1) l1 l2

  (* only for represent (int/bool) values*)
  | Int(n) -> string_of_int n
  | Bool(b) -> string_of_bool b
  | Label(s) -> s ^ ": \n"