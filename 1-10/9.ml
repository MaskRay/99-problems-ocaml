let rec group =
  let ($) f g x = f (g x) in
  let rec go run acc = function
    | [] -> []
    | [a] -> (a::run)::acc
    | a :: (b :: _ as xs) -> if a = b then go (a::run) acc xs else go [] ((a::run)::acc) xs
  in List.rev $ go [] []
