type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let complete_binary_tree n =
  let rec go i =
    if i > n then Leaf
    else Branch (i, go (i*2), go (i*2+1))
  in go 1

let is_complete_binary_tree t =
  let rec go = function
    | Leaf -> true, 0, 0
    | Branch (_, l, r) ->
      let bl, minl, maxl = go l in
      let br, minr, maxr = go r in
      bl && br && maxl >= minl && minl >= maxr && maxr >= minr && maxl-1 <= minr, 1 + min minl minr, 1 + max maxl maxr
  in let res, _, _ = go t in
     res
