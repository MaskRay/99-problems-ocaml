type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let ($) f g x = f (g x)

let rec add x = function
  | Leaf -> Branch (x, Leaf, Leaf)
  | Branch (y, l, r) ->
    if x < y then Branch (y, add x l, r)
    else Branch (y, l, add x r)

let is_symmetric t =
  let rec go = function
    | Leaf, Leaf -> true
    | Leaf, Branch _
    | Branch _, Leaf -> false
    | Branch (_, al, ar), Branch (_, bl, br) -> go (al, br) && go (ar, bl)
  in go (t, t)

let construct = List.fold_left (fun t x -> add x t) Leaf

let test_symmetric = is_symmetric $ construct
