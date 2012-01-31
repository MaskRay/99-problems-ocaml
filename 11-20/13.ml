type 'a elem = One of 'a | Many of int * 'a

let ($) f g x = f (g x)

let rec encode_direct =
  let f c x = if c == 1 then One x else Many (c,x) in
  let rec go c acc = function
    | [] -> []
    | [a] -> f (c+1) a :: acc
    | a :: (b :: _ as xs) -> if a = b then go (c+1) acc xs else go 0 (f (c+1) a :: acc) xs
  in List.rev $ go 0 []
