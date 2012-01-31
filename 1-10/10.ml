let rec encode =
  let ($) f g x = f (g x) in
  let rec go c acc = function
    | [] -> []
    | [a] -> (c+1,a)::acc
    | a :: (b :: _ as xs) -> if a = b then go (c+1) acc xs else go 0 ((c+1,a)::acc) xs
  in List.rev $ go 0 []
