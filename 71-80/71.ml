type 'a rose_tree = { root : 'a; sub_forest : 'a rose_tree list }

let ipl =
  let rec sum = function [] -> 0 | (x :: xs) -> x + sum xs in
  let rec go d t = d + sum (List.map (go (d+1)) t.sub_forest) in
  go 0
