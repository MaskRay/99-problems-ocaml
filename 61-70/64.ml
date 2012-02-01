type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let layout =
  let rec go x y = function
    | Leaf -> (Leaf, x)
    | Branch (a,l,r) ->
      let l', x' = go x (y+1) l in
      let r', x'' = go (x'+1) (y+1) r in
      (Branch ((a,x',y), l', r'), x'')
  in go 1 1

let example =
Branch ('d',
	Branch ('b',
		Branch ('a', Leaf, Leaf),
		Branch ('c', Leaf, Leaf)),
	Branch ('e',
		Branch ('f', Leaf, Leaf),
		Leaf))
