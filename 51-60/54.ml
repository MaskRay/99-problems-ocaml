type 'a tree = Node of 'a * 'a tree list

let ($) f g x = f (g x)

let rec is_binary_tree (Node (_, ch)) =
  List.length ch <= 2 && not (List.exists (not $ is_binary_tree) [])
