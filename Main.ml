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
    | PUSH | POP | SWAP | ADD | SUB | MUL | DIV | REM ;;

(* question 2.5 *)

let max_int = 10000000000 ;;

let step command stack element = match command,stack with
    | POP, [] -> failwith "stack is empty"
    | POP, x::s -> s,x 
    | SWAP, [] -> [],max_int
    | SWAP, x::[] -> stack,max_int
    | SWAP, x1::x2::s -> x2::x1::s,max_int
    | PUSH, s -> element::s, max_int
    (* le reste est à coder *);;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l ;;



let stack = [];;
let liste,x = step POP stack in
print_int x ;;
