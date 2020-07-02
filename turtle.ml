Printexc.record_backtrace true;;

let sw = 800;;
let sh = 600;;

open Graphics;;
open_graph "";;
open Printf;;
open List;;
open Buffer;;
open Char;;
open Tgtypes;;

let prnflush s = printf s; flush stdout;;

resize_window 800 600;;

let procs = Hashtbl.create 10
let procargs = Hashtbl.create 20


type state2d = 
  {
  mutable running: bool;
  mutable rc:int; 
  mutable x:int; 
  mutable y: int; 
  mutable pendown: bool;
  mutable dir: int;
  mutable vars: (string, int) Hashtbl.t;
  };;

let callstates: state2d Stack.t = Stack.create()

let copystate s =
  let st = ref { vars = (Hashtbl.copy s.vars);  
               rc=0;
               running = true;
               x=s.x; y=s.y; pendown=s.pendown; dir=s.dir;} in
               st;;

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t;;

let move (dist:int) (state:state2d ref) =
  let s = !state in
  let d = float_of_int dist in
  let rad = (float_of_int s.dir) *. Float.pi /. 180.0 in
  s.x <- s.x + int_of_float(d *. cos(rad));
  s.y <- s.y + int_of_float(d *. sin(rad));;
 
(* let int_of_bool b = if b then 1 else 0;; *)

let rec exec (cmd:exprnode) (state:state2d ref) =
  printf "Stack size: %d\n" (Stack.length callstates);
  match cmd with
  | Multiply (Num a, Num b) -> Num (a*b);                               
  | Multiply (x, y) -> exec (Multiply ( (exec x state),(exec y state)) ) state
  | Add (Num a, Num b) -> Num (a+b);                               
  | Add (x, Num y) -> exec (Add ( (exec x state), Num y) ) state
  | Add (Num x, y) -> exec (Add ( (exec y state), Num x) ) state
  | Add (x, y) -> exec (Add ( (exec x state),(exec y state)) ) state
  | Divide (Num x, Num y) -> Num (x/y)
  | Divide (x, y) -> exec (Divide ( (exec x state),(exec y state)) ) state
  | Subtract (Num a, Num b) -> Num (a-b);                               
  | Subtract (x, Num y) -> exec (Subtract ( (exec x state), Num y) ) state
  | Subtract (Num x, y) -> exec (Subtract ( (exec y state), Num x) ) state
  | Subtract (x, y) -> exec (Subtract ( (exec x state),(exec y state)) ) state
  | Forward (Num n) -> if not !state.pendown then moveto !state.x !state.y;
                       move n state;
                       if !state.pendown then (
                         lineto !state.x !state.y; 
                         moveto !state.x !state.y
                       );
                       Num 0;
  | Forward x -> exec (Forward (exec x state)) state 
  | Backward (Num n) -> if not !state.pendown then moveto !state.x !state.y;
                       move (-1*n) state;
                       if !state.pendown then ( 
                         lineto !state.x !state.y; 
                         moveto !state.x !state.y
                       );
                       Num 0;
  | Backward x -> exec (Backward (exec x state)) state 
  | TurnRight Num d -> !state.dir <- !state.dir - d; Num 0
  | TurnLeft Num d -> !state.dir <- !state.dir + d; Num 0
  | TurnRight x -> exec (TurnRight (exec x state)) state
  | TurnLeft x -> exec (TurnRight (exec x state)) state
  | PenDown -> (!state.pendown <- true; printf "pen down\n"; ); Num 0
  | Block l -> printf "Found block. Running \n";flush stdout; run l state
  | Operator _ -> printf "found op\n"; Num 0
  | Condition (op, l, r) -> let el = (exec l state) in
                            let er = (exec r state) in
                              (match (el, er) with
                              | (Num a, Num b) -> printf "found nums cond\n";
                                (match op with
                                  | Equal -> if a = b then Num 1 else Num 0
                                  | LessThan -> if a < b then Num 1 else Num 0
                                  | GreaterThan -> if a > b then Num 1 else Num 0
                                )
                              | (_, _) -> Num 0)
  | If ((o,cl,cr),l) -> ignore l; let ok = (exec (Condition (o,cl,cr)) state) in 
       if ok = Num 1 then (
         printf "condition true\n";
         run l state
       )
       else (printf "condition false\n"; Num 0) 
  | Repeat (Num n,l) -> for i = 1 to n do ignore i;ignore (run l state) done; Num 0
  | Repeat (e, l) -> exec (Repeat ((exec e state), l)) state 
  | Proc (name, al, sl) -> printf "Procedure has %d statements\n" (List.length sl);
                           Hashtbl.add procs name sl;
                           Hashtbl.add procargs name al; Num 0
  | Call (name, fl) -> printf "Call() has %d params passed to it\n"
                       (List.length fl);
                       let args = Hashtbl.find procargs name in
                       let newst = copystate !state in
                       List.iteri (fun i a -> 
                         printf "arg..\n";
                         let valn = List.nth fl i in
                         match valn with
                           | Num nn -> Hashtbl.add !newst.vars a nn; printf "Arg %s = %d\n" a nn
                           | x -> printf "Exec in call arg\n";let res = exec x newst in
                             match res with
                               | Num n -> Hashtbl.add !newst.vars a n; printf "Arg %s = %d\n" a n
                               | _ -> printf "unknown arg type";
                       ) args ;
                       Stack.push !newst callstates;
                       printf "Hello there\n";
                       let procstatements = Hashtbl.find procs name in
                       let ret = run procstatements newst in
                       printf "procstatements len is %d\n" (List.length procstatements);
                       ignore (Stack.pop callstates);
                       ret
  | Print Num n -> printf "-------------> %d\n" n; Num 0
  | Print e -> exec (Print (exec e state)) state
  | Num n -> Num n
  | Stop -> Stop
  | Var x -> ( (* printf "lookup var %s\n" x.name; *)
             let vars = !state.vars in
             let res = (Hashtbl.find_opt vars x.name) in
             match res with
             | None -> printf "could not find %s size is %d" x.name (Hashtbl.length vars);Num 0
             | Some n -> Num n
             ) 
  | Param s -> ignore s;printf "Found param\n"; Num 0
  | _ -> printf "other\n"; (Num 99)


and run program st =
  printf "-------- RUN -------\n";
  flush stdout;
  printf "len=%d\n" (List.length program);
  flush stdout;
  printf "---- Listing -----\n";
  flush stdout;
  listall program st;
  (* if !st.rc = 0 then (
    moveto !st.x !st.y
  ); *)
  match program with
  |  [] -> !st.rc <- 0; printf "Done.\n"; flush stdout; Num 0
  |  Stop::rest -> ignore rest;printf "Stack size=%d\n" (Stack.length callstates);printf "Stop.\n";flush stdout;Stop
  |  stmt::rest -> printf "st::rst Stack size=%d\n" (Stack.length callstates);flush stdout;
                   let ok = (exec stmt st) in
                   if ok == Stop then (
                     printf "Found Stop\n";
                     flush stdout;
                     Stop)
                   else (
                      !st.rc <- !st.rc + 1;
                      printf "next statement\n";
                      flush stdout;
                      run rest st);;


let st = ref { vars = Hashtbl.create 10; rc=0;
               running = true;
                x=250; y=250; pendown=false; dir=0;};;

let process (line : string) =
  let linebuf = Lexing.from_string line in
  set_color green;
  moveto 250 250;
  try
    !st.running <- true;
    let statements = [(Parser.main Lexer.token linebuf);] in
    printf "Parse output has %d list items.\n" (List.length statements);
    run statements st
  with
  | Lexer.Error msg ->
      fprintf stderr "%s%!" msg; Num 0
  | Parser.Error ->
      fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf); Num 0

let process (optional_line : string option) =
  match optional_line with
  | None ->
      Num 0
  | Some line ->
      process line

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  ignore (process optional_line);
  if continue then
    repeat channel
  
let repl () =
  prnflush "Interpreter ready.\n";
  repeat (Lexing.from_channel stdin);;
