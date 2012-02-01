let rec gray = function
  | 0 -> [0]
  | n -> let mul2 = fun x -> x * 2 in
	 let ($) f g x = f (g x) in
	 let g = gray (n-1) in
	 List.map (mul2) g @ List.map (succ $ mul2) (List.rev g)
