%token <int> INT
%token <float> FLOAT
%token <string> ID
%token FORWARD
%token BACKWARD
%token PENDOWN
%token PENUP
%token PRINT
%token REPEAT
%token END
%token STOP
%token LESSTHAN
%token EQUAL
%token GREATERTHAN
%token TO
%token IF
%token TURNRIGHT
%token TURNLEFT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token SEMICOLON
%token COLON

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Tgtypes.exprnode> main

%%

main:
| e = expr EOL
    { e }

argu:
| COLON; s = ID
    { s }

conid:
| l = expr; EQUAL; r = expr
    { (Equal, l, r) }
| l = expr; LESSTHAN; r = expr
    { (LessThan, l, r) }
| l = expr; GREATERTHAN; r = expr
    { (GreaterThan, l, r) }

expr:
| i = INT
    { Num i }
| LPAREN; sl = list_statements; RPAREN
    { Block sl }
| REPEAT; e = expr; LPAREN; sl = list_statements; RPAREN
    { Repeat (e, sl) }
| TO; s = ID; al = list_args; sl = list_statements; END
    { Proc (s, al, sl) }
| IF; c = conid; LPAREN; el = list_statements; RPAREN
    { If (c, el) }
| FORWARD; e = expr
    { Forward e }
| BACKWARD; e = expr
    { Backward e }
| TURNRIGHT; e = expr
    { TurnRight e }
| TURNLEFT; e = expr
    { TurnLeft e }
| PENDOWN
    { PenDown }
| PENUP
    { PenUp }
| STOP
    { Stop }
| e1 = expr TIMES e2 = expr
    { Multiply (e1, e2) }
| e1 = expr DIV e2 = expr
    { Divide (e1, e2) }
| e1 = expr PLUS e2 = expr
    { Add (e1, e2) }
| e1 = expr MINUS e2 = expr
    { Subtract (e1, e2) }
| s = ID; LPAREN; f = list(expr); RPAREN
    { Call (s, f) }
| COLON; s = ID
    { Var {name = s; t=IntT } }
| PRINT; e = expr
    { Print e }
(* | MINUS e = expr %prec UMINUS
    { -. e } *)

list_statements:
    vl = list(expr)
       { vl }

list_args:
    al = list(argu)
       { al }