let prime_factors_mult =
  let rec go d n =
    if n = 1 then [] else
      if n mod d = 0 then
	match go d (n/d) with
	  | (d',c) :: t when d' = d -> (d',c+1) :: t
	  | t -> (d,1) :: t
      else
	go (d+1) n
  in go 2
