type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec tree_string = function
  | Leaf -> ""
  | Branch (x,l,r) -> String.concat "" [tree_string l; String.make 1 x; tree_string r]
