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

  | Int of int
  | Bool of bool
  | Label of string