type 'a nested = Elem of 'a | List of 'a nested list

let flatten xs =
  let rec go acc = function
    | [] -> acc
    | Elem x :: xs -> go (x::acc) xs
    | List x :: xs -> go (go acc x) xs
  in List.rev (go [] xs)

