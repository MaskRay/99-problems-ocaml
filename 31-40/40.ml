let is_prime n =
  let rec go x = x*x > n || n mod x <> 0 && go (x+1)
  in n > 1 && go 2

let goldbach n =
  let rec go d =
    if is_prime d && is_prime (n-d) then
      (d, n-d)
    else
      go (d+1)
  in go 2
