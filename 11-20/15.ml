let repli xs n =
  let rec replicate acc x = function
    | 0 -> acc
    | n -> replicate (x::acc) x (n-1)
  in List.flatten (List.map (fun x -> replicate [] x n) xs)
