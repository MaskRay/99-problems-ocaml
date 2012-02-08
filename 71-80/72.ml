type 'a rose_tree = { root : 'a; sub_forest : 'a rose_tree list }

let bottom_up t =
  let flip f x y = f y x in
  let rec go t acc = List.fold_right go t.sub_forest (t.root :: acc)
  in go t []
