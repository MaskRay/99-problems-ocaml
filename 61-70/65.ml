type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> -1
  | Branch (_, l, r) -> 1 + max (depth l) (depth r)

let rec root_x h = function
  | Leaf -> 1
  | Branch (_, l, r) -> root_x (h-1) l + 1 lsl (h-1)

let layout t =
  let d = depth t in
  let rec go x y = function
    | Leaf -> Leaf
    | Branch (a,l,r) ->
      Branch ((a,x,y), go (x-1 lsl (d-y)) (y+1) l, go (x+1 lsl (d-y)) (y+1) r)
  in go (root_x d t) 1 t
