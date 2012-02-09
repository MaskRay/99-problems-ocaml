let rec comb n xs = match n, xs with
  | 0, xs -> [[], xs]
  | n, [] -> []
  | n, x::xs ->
    let ps = [? List : (x::ys, zs) | (ys, zs) <- List : comb (n-1) xs ?] in
    let qs = [? List : (ys, x::zs) | (ys, zs) <- List : comb n xs ?] in
    ps @ qs

let rec group ns xs = match ns, xs with
  | [], _ -> [[]]
  | n::ns, xs ->
    [? List : g::gs | (g, ys) <- List : comb n xs; gs <- List : group ns ys ?]
