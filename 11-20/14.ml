let dupli =
  let rec go acc = function
    | [] -> acc
    | x :: xs -> go (x::x::acc) xs
  in List.rev $ go []
