open MiniImp

(* Define the type for memory as a map from strings (variable names) to values *)
type memory

(* Lookup the value of a variable in memory *)
val lookup : memory -> string -> value option
(* Update the value of a variable in memory *)
val update : memory -> string -> value -> memory


val eval_exp : memory -> exp -> value 
(* Evaluate a statement, returning the updated memory *)
val eval_stmt : memory -> stmt -> memory
