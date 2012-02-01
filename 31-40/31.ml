let is_prime n =
  let rec go x = x*x > n || n mod x <> 0 && go (x+1)
  in n > 1 && go 2
