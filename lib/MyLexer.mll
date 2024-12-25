{
    open MyParser
    exception LexingError of string
}

(* regexp *)
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"
let integer = '-'?['0' - '9']['0' - '9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* lexing rules *)
rule read = parse
| "def"     {DEF}
| "main"    {MAIN}
| "with"    {WITH}
| "input"   {INPUT}
| "output"  {OUTPUT}
| "as"      {AS}

| ";" {SEMI}

| "+" {PLUS}
| "-" {MINUS}
| "*" {TIMES}
| "and" {AND}
| "not" {NOT}
| "<" {MINOR}
| "=" {ASSIGN}

| "(" {OPEN_PAR}
| ")" {CLOSE_PAR}
| "{" {OPEN_CURLY}
| "}" {CLOSE_CURLY}

| "if" {IF}
| "then" {THEN}
| "else" {ELSE}

| "while" {WHILE}
| "do" {DO}
| eof {EOF}

| whitespace {read lexbuf}
| integer {INT (int_of_string (Lexing.lexeme lexbuf))}
| bool {BOOL (bool_of_string (Lexing.lexeme lexbuf))}
| id {VAR (Lexing.lexeme lexbuf)}

| _ as c { raise (LexingError (Printf.sprintf "Unexpected character '%c' at position %d" c (Lexing.lexeme_start lexbuf))) }