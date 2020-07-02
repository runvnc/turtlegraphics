open turtle;;

let list = (cmd:exprnode) (state:state2d ref) =
  ignore state;
  match cmd with                              
  | Multiply _ -> printf "Mult "
  | Add _ -> printf "Add "
  | Divide _ -> printf "Div "                               
  | Subtract _ -> printf "Sub "
  | Forward _ -> printf "Forward " 
  | Backward _ -> printf "Backward " 
  | TurnRight _ -> printf "TurnRight "
  | TurnLeft _ -> printf "TurnLeft "
  | PenDown -> printf "PenDown "
  | Block _ -> printf "Block "
  | Operator _ -> printf "Op "
  | Condition _ -> printf "Cond "
  | If _ -> printf "If " 
  | Repeat _ -> printf "Repeat " 
  | Proc _ -> printf "Proc "
  | Call _ -> printf "Call "
  | Print Num n -> printf "-------------> %d\n" n
  | Print _ -> printf "Print "
  | Num _ -> printf "Num "
  | Stop -> printf "Stop "
  | Var _ -> printf "Lookup \n"
  | Param _ -> printf "Param\n"
  | _ -> printf "Other "


and listall program st =
 match program with
  |  [] -> printf "END PROG.\n"; flush stdout
  |  stmt::rest -> list stmt st;
                   listall rest st