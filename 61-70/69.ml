type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec preorder = function
  | Leaf -> "."
  | Branch (x,l,r) -> String.concat "" [String.make 1 x; preorder l; preorder r]

let build s =
  let rec tree i =
    if s.[i] = '.' then
      Leaf, i+1
    else
      let l, j = tree (i+1) in
      let r, k = tree j in
      Branch (s.[i], l, r), k
  in fst (tree 0)
