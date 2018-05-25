NB : pour faire ø : alt gr + o




## Exercice 1

### question 1.1

Une pile est une sructure de donnée de type Lifo (last in first out).
On peut généralement lui appliquer les opérations push, pop, swap, empty (pour savoir si elle est vide), et top (pour avoir le premier élément de la pile sans le détruire).



## Exercice 2

### question 2.1

* sémantique 1 : Si le nombre d'arguments donné est différent du nombre d'arguments attendu, alors une erreur est retournée.

* sémantique 2 : Si au cours de l'application de la séquence d'instructions Q à la pile v1::...::vn::ø donne une erreur, alors au cours de l'éxecution du programme i,Q avec les arguments v1, ..., vn, une erreur sera retournée.

* sémantique 3 : Si l'application de la séquenece Q à la pile donnée à n éléments donne la pile v::S, alors l'éxecution du programme n,Q avec les arguments v1, ..., vn retourne la valeur v (sommet de la pile).

### question 2.2

Le comportement lorsque la pile est vide. Que se passe-t-il s'il y a plus d'instructions que de valeurs dans la pile ? On imaginera qu'une erreur est alors retournée. On peut en déduire la règle suivante :
```
(4)    Q, v1:: ...:: vn :: ø -> Q',ø
      -------------------------------
      v1, ..., vn |- n,Q => ERR
```

### question 2.3


* pour le pop :
```
(1)   pop.Q, v1::S -> Q, S
```

* pour le push :
```
(2)   (push x).Q, S -> Q, x::S
```

* pour le swap :
```
(3)   swap.Q, v1::v2::S -> Q, v2::v1::S
```

* pour les opérations arithmétiques/binaires :
```
(4)   op in {add,mul,div} r=v1 op v2
     -------------------------------------------------
      op.Q, v1::v2::Q -> r::S
```

* pour l'erreur de div et rem :
```
(5)  od in {div,rem}
    ------------------
     od.Q,v1::0::S -> ERR
```

* pour l'erreur de pop :
```
         S = ø
    ---------------
     pop.Q,S -> ERR
```
### question 2.4
```
type command = 
    | PUSH of int | POP | SWAP | ADD | SUB | MUL | DIV | REM ;;
```
### question 2.5


```
let max_int = 10000000000 ;;

let step command stack = match command,stack with
    | POP, [] -> failwith "stack is empty"
    | POP, x::s -> s,x 
    | SWAP, [] -> [],max_int
    | SWAP, x::[] -> stack,max_int
    | SWAP, x1::x2::s -> x2::x1::s,max_int
    | PUSH(e), s -> e::s, max_int
    (* le reste est à coder *);;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l ;;



let stack = [];;
let liste,x = step POP stack in
print_int x ;;
```



## Exercice 3

### question 3.1

* Schéma de compilation Expr -> PFx :

1. Const(i) -> PUSH i
2. pour Var(s) -> PUSH valeur_associée_à_Var(s)()  
3. Binop(op,exp,exp) -> PUSH valeur_associée_à_Exp(exp1) PUSH valeur_associée_à_Exp(exp2) op
4. Uminus(exp) -> PUSH (-1) PUSH valeur_associée_à_Exp(exp) MUL 

* Description formelle : 
Notons E la fonction d'environnement qui associe la valeur d'une
1. E-| Const i -> PUSH i 
2.
```   
       x € dom(E)
 ----------------------
    E -| Var x => E(x)
```
3.
```   
     E-|e1 =>v1   E-|e2 =>v2   OP|-binop=>op
 ----------------------------------------------------
    E,BINOP -| op(e1, e2) => PUSH v1 PUSH v2 op
```

4.
```   
                   E-|e1 =>v1
 ----------------------------------------------------
    E -| -exp => PUSH 0 PUSH 1 SUB PUSH valeur_associée_à_Exp(exp) MUL
```

### question 3.2
```
let rec eval env exp = match exp with
	| Const c -> c
	| Var v -> (try List.assoc v env with Not_found -> raise(Unbound_variable v))
	;;

let generate_binop bop = match bop with
	| '+' -> "ADD"
	| '-' -> "SUB"
	| '*' -> "MUL"
	| '/' -> "DIV"
	| '%' -> "REM" ;;

let generate env expression = match expression with
	| Const c -> "PUSH"^(eval expression)
	| Var v -> "PUSH"^(eval env expression)
	| Binop(op,e1,e2) -> (generate env e1)^(generate env e2)^(generate_binop op)
	| Uminus e -> "PUSH0"^"PUSH1"^"SUB"^"PUSH"^(generate env e)^"MUL" ;;
```
