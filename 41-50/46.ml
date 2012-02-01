type exp =
  | And of exp * exp
  | Or of exp * exp
  | Nand of exp * exp
  | Xor of exp * exp
  | Impl of exp * exp
  | Equ of exp * exp
  | A
  | B

let eval a b =
  let rec f = function
    | A -> a
    | B -> b
    | And (x,y) -> f x && f y
    | Or (x,y) -> f x || f y
    | Nand (x,y) -> not (f x && f y)
    | Xor (x,y) -> f x <> f y
    | Impl (x,y) -> not (f x) || f y
    | Equ (x,y) -> f x = f y
  in f

let table exp =
  let p a b =
    print_string (string_of_bool a);
    print_char ' ';
    print_string (string_of_bool b);
    print_char ' ';
    print_endline (string_of_bool (eval a b exp))
  in
  p false false;
  p false true;
  p true false;
  p true true
