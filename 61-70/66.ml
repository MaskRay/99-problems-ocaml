type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec pad x y = match x, y with
  | [], y | y, [] -> y
  | x::xs, y::ys -> x :: pad xs ys

let rec maximum = function
  | [x] -> x
  | h::t -> max h (maximum t)

let rec max_sum = function
  | [], _ | _, [] -> 0
  | a::b, c::d -> max (a+c) (max_sum (b, d))

let layout t =
  let rec go = function
    | Leaf -> ([], Leaf, [])
    | Branch (a, l, r) ->
      let ll, l', lr = go l in
      let rl, r', rr = go r in
      let sep = max_sum (lr, rl) / 2 + 1 in
      ( 0 :: pad (List.map ((+) sep) ll) (List.map (fun p -> p-sep) rl)
      , Branch ((a, sep), l', r')
      , 0 :: pad (List.map ((+) sep) rr) (List.map (fun p -> p-sep) lr)
      ) in
  let rec go2 x y = function
    | Leaf -> Leaf
    | Branch ((a, dx), l, r) ->
      Branch ( (a, x, y)
	     , go2 (x-dx) (y+1) l
             , go2 (x+dx) (y+1) r) in
  let l, t', r = go t in
  go2 (maximum l + 1) 1 t'
