type command = PUSH | POP | SWAP | ADD | SUB | MUL | DIV | REM;;

let step command stack element = match command, stack, element with 
| PUSH, h::t, e -> e::h::t
(*| POP, v1::v2, [] -> []
| SWAP, v1::v2 -> [] 
| ADD, v1::v2 -> []
| SUB, v1::v2 -> []
| MUL, v1::v2 -> []
| DIV, v1::v2 -> []
| REM, v1::v2 -> []
| REM, [] -> failwith "error"
| DIV, [] -> failwith "error"
| MUL, [] -> failwith "error"
| SUB, [] -> failwith "error"
| ADD, [] -> failwith "error"
| SWAP, [] -> failwith "error"
| POP, [] -> failwith "error"
| PUSH, [] -> failwith "error"
*)
;;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let st = 4::3::74::[];;
print_char '\n'; print_list (step PUSH st 10); print_char '\n'; print_char '\n';;

