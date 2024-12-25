%{
    open MiniImp
%}

%token EOF SEMI


%token DEF MAIN WITH INPUT OUTPUT AS 
%token <int> INT
%token PLUS MINUS TIMES

%token <bool> BOOL
%token AND NOT MINOR

(* %token <string> STRING *)
%token WHILE DO
%token IF THEN ELSE

%token <string> VAR 
(* %token <string> ARITH_VAR BOOL_VAR *)
%token ASSIGN

%token OPEN_PAR CLOSE_PAR
%token OPEN_CURLY CLOSE_CURLY

(* starting symbol*)
%start <MiniImp.program> prog

%type <MiniImp.stmt> stmt stmt_list
%type <MiniImp.exp> exp
%type <MiniImp.exp> arit_exp
%type <MiniImp.exp> bool_exp



(* precedence *)
(* %left SEMI *)
(* %nonassoc MINOR *)
%left PLUS MINUS
%left TIMES AND
%nonassoc NOT

%%

prog:
    | DEF MAIN WITH INPUT id1=VAR OUTPUT id2=VAR AS body=stmt_list EOF  {Main(id1, id2, body)} 

stmt_list:
    | t1=stmt SEMI t2=stmt_list                         {Seq(t1, t2)}
    | t1=stmt                                           {t1}


stmt:
    | IF cond=bool_exp THEN OPEN_CURLY t1=stmt_list CLOSE_CURLY ELSE OPEN_CURLY t2=stmt_list CLOSE_CURLY    {If(cond, t1, t2)}
    | WHILE cond=bool_exp DO OPEN_CURLY t1=stmt_list CLOSE_CURLY                                            {While(cond, t1)}
    | id=VAR ASSIGN e=exp                                                                                   {Assign(id, e)}
    | OPEN_PAR t=stmt_list CLOSE_PAR                                                                        {t}

exp:
    | t=arit_exp                    {t}
    | t=bool_exp                    {t}

(*Report: space need to disambiguate between - as unary operator (-1) and binary operation (x - 1)*)
arit_exp:
    | id=VAR                            {Var(id)}
    | i=INT                             {Aval(i)}
    | t1=arit_exp PLUS t2=arit_exp      {Plus(t1, t2)}
    | t1=arit_exp MINUS t2=arit_exp     {Minus(t1, t2)}
    | t1=arit_exp TIMES t2=arit_exp     {Times(t1, t2)}
    | OPEN_PAR e=arit_exp CLOSE_PAR     {e}

(*boolean cannot have vars; only literals*)
bool_exp:
    | b=BOOL                            {Bval(b)}
    | t1=bool_exp AND t2=bool_exp       {And(t1, t2)}
    | t1=arit_exp MINOR t2=arit_exp     {Minor(t1, t2)}
    | NOT t1=bool_exp                   {Not(t1)}
    | OPEN_PAR e=bool_exp CLOSE_PAR     {e}

