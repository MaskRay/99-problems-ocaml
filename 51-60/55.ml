type tree = Leaf | Branch of tree * tree

let rec cbal_tree = function
  | 0 -> Leaf
  | n -> Branch (cbal_tree (n/2), cbal_tree ((n-1)/2))
