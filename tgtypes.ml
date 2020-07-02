type mytypes = IntT | FloatT
and variable = { name:string; t: mytypes }

and op = LessThan | Equal | GreaterThan
and cond = op  * exprnode * exprnode
and exprnode =
  | Matrix of float array array
  | Num of int
  | Var of variable
  | Multiply of exprnode * exprnode
  | Divide of exprnode * exprnode
  | Add of exprnode * exprnode
  | Subtract of exprnode * exprnode
  | Forward of exprnode
  | Backward of exprnode
  | TurnRight of exprnode
  | TurnLeft of exprnode
  | Repeat of repeat 
  | Block of exprnode list
  | Proc of string * string list * exprnode list
  | PenUp
  | PenDown
  | Float of float
  | Call of string * exprnode list
  | Param of string
  | Print of exprnode
  | Operator of op
  | Condition of op * exprnode * exprnode 
  | If of cond * exprnode list
  | Stop
and repeat = exprnode * exprnode list 

