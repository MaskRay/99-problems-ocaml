type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec leaves = function
  | Leaf -> []
  | Branch (x, Leaf, Leaf) -> [x]
  | Branch (x, l, r) -> leaves l @ leaves r
