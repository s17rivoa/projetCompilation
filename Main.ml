(* Entry point of the program, should contain your main function: here it is
 named compile, it is the function provided after question 4.1 *)

let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    (* work on the lexbuf *)
    close_in (input_file)
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")

(* The following line should be uncommented and adapted after question 5.1: *)
(* let _ = Arg.parse [] compile "" *)


type command = 
    | PUSH of int | POP | SWAP | ADD | SUB | MUL | DIV | REM ;;

(* question 2.5 *)

let max_int = 10000000000 ;;

let step command stack = match command,stack with
    | POP, [] -> failwith "stack is empty"
    | POP, x::s -> s,x 
    | SWAP, [] -> [],max_int
    | SWAP, x::[] -> stack,max_int
    | SWAP, x1::x2::s -> x2::x1::s,max_int
    | PUSH(e), s -> e::s, max_int
    | ADD, x1::x2::s -> (x1+x2)::s, max_int
    | ADD, _ -> failwith "stack too short"
    | SUB, x1::x2::s -> (x1-x2)::s, max_int
    | SUB, _ -> failwith "stack too short"
    | MUL, x1::x2::s -> (x1*x2)::s, max_int
    | MUL, _ -> failwith "stack too short"
    | DIV, x1::0::s -> failwith "math error"
    | DIV, x1::x2::s -> (x1/x2)::s, max_int
    | DIV, _ -> failwith "stack too short"
    | REM, x1::0::s -> failwith "math error"
    | REM, x1::x2::s -> (x1 mod x2)::s, max_int
    | REM, _ -> failwith "stack too short";;


let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l ;;

(*Test*)
(*let st = 10::0::3::74::[];; (*stack initielle*)
let res_list, l = (step ADD st);;
print_char '\n'; print_list res_list ; print_char '\n'; print_char '\n';; *)

type expression =
  | Const of int
  | Var of string
  | Binop of char * expression * expression (*BinOp.t * expression * expression*)
  | Uminus of expression ;;

let eval env exp = match exp with
   | Const(c) -> string_of_int c
   | Var v -> (try string_of_int(List.assoc v env) with Not_found -> failwith ("Unbound variable "^v))
   ;;

let generate_binop bop = match bop with
   | '+' -> "ADD"
   | '-' -> "SUB"
   | '*' -> "MUL"
   | '/' -> "DIV"
   | '%' -> "REM" ;;

let rec generate env expression = match expression with
   | Const c -> "PUSH"^(eval env expression)
   | Var v -> "PUSH"^(eval env expression)
   | Binop(op,e1,e2) -> (generate env e1)^(generate env e2)^(generate_binop op)
   | Uminus e -> "PUSH0"^"PUSH1"^"SUB"^"PUSH"^(generate env e)^"MUL" ;;

(* test *)
(*let env = [("coucou",45) ; ("miaou", 78)] in
print_char '\n' ; print_string(generate env (Binop('+',Var("coucou"),Binop('*',Var "miaou",Const 1))) );; *)
