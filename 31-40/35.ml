let prime_factors n =
  let rec go d n =
    if n mod d = 0 then
      d :: go d (n/d)
    else if n = 1 then
      []
    else if d*d > n then
      [n]
    else
      go (d+1) n
  in go 2 n
