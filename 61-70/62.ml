type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec internals = function
  | Leaf | Branch (_, Leaf, Leaf) -> []
  | Branch (x, l, r) -> x :: internals l @ internals r
