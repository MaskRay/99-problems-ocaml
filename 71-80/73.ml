type 'a rose_tree = { root : 'a; sub_forest : 'a rose_tree list }

let string_to_list s =
  let rec go x =
    if x >= String.length s then []
    else s.[x] :: go (x+1)
  in go 0

let parse s =
  let rec tree t = match t with
    | '(' :: x :: xs -> let sub, ys = forest xs
			in {root = x; sub_forest = sub}, ys
    | x :: xs -> {root = x; sub_forest = []}, xs
  and forest = function
    | ')' :: xs -> [], xs
    | xs -> let t, ys = tree xs in
	    let ts, zs = forest ys in
	    t :: ts, zs
  in fst (tree (List.filter (fun c -> c<>' ') (string_to_list s)))

let rec display t =
  if t.sub_forest = [] then
    print_char t.root
  else begin
    print_char '(';
    print_char t.root;
    List.map (fun t -> print_char ' '; display t) t.sub_forest;
    print_char ')'
  end
