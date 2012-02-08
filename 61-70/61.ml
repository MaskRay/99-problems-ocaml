type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec count_leaves = function
  | Leaf -> 0
  | Branch (x, Leaf, Leaf) -> 1
  | Branch (x, l, r) -> count_leaves l + count_leaves r
