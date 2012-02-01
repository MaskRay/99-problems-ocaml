let totient_phi n =
  let rec decom n d =
    if n mod d = 0 then
      let n', m' = decom (n/d) d in
      (n', m' * d)
    else
      (n, 1)
  in
  let rec go acc d n =
    if n = 1 then
      acc
    else if d*d > n then
      (n-1) * acc
    else if n mod d = 0 then
      let n', mul = decom (n/d) d in
      go (mul*(d-1)*acc) (d+1) n'
    else
      go acc (d+1) n
  in go 1 2 n;;

print_endline (string_of_int (totient_phi 10090))
