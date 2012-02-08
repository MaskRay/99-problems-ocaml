type 'a rose_tree = { root : 'a; sub_forest : 'a rose_tree list }

let rec nnodes t = List.fold_left (+) 1 (List.map nnodes t.sub_forest)
