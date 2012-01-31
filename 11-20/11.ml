type 'a elem = One of 'a | Many of int * 'a

let ($) f g x = f (g x)

let rec encode =
  let rec go c acc = function
    | [] -> []
    | [a] -> (c+1,a)::acc
    | a :: (b :: _ as xs) -> if a = b then go (c+1) acc xs else go 0 ((c+1,a)::acc) xs
  in List.rev $ go 0 []

let encode_modified =
  List.map (function
    | (1,x) -> One x
    | (c,x) -> Many (c,x)) $ encode
