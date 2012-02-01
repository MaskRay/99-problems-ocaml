type tree = Leaf | Branch of tree * tree

let is_symmetric t =
  let rec go = function
    | Leaf, Leaf -> true
    | Leaf, Branch _
    | Branch _, Leaf -> false
    | Branch (al, ar), Branch (bl, br) -> go (al, br) && go (ar, bl)
  in go (t, t)
