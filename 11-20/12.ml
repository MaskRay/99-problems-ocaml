type 'a elem = One of 'a | Many of int * 'a

let ($) f g x = f (g x)

let decode_modified =
  let rec replicate acc x = function
    | 0 -> acc
    | n -> replicate (x::acc) x (n-1)
  in List.flatten $ List.map (function
    | One x -> [x]
    | Many (c,x) -> replicate [] x c)
