let rev =
  let rec go acc = function
    | [] -> acc
    | x::xs -> go (x::acc) xs
  in go []
