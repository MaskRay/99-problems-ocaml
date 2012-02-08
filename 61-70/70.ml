type 'a rose_tree = { root : 'a; sub_forest : 'a rose_tree list }

let string_to_list s =
  let rec go x =
    if x >= String.length s then []
    else s.[x] :: go (x+1)
  in go 0

let parse s =
  let rec tree (x::xs) = let sub, ys = forest xs
			 in {root = x; sub_forest = sub}, ys
  and forest = function
    | ('^' :: xs) -> [], xs
    | xs -> let t, ys = tree xs in
	    let ts, zs = forest ys in
	    t :: ts, zs
  in fst (tree (string_to_list s))

let rec display t =
  print_char t.root;
  List.map display t.sub_forest;
  print_char '^';;
