let drop xs n =
  let rec go x = let x' = x + 1 in
		 function
		   | [] -> []
		   | b::bs -> if x' == n then go 0 bs else b :: go x' bs
  in go 0 xs
