let is_prime n =
  let rec go x = x*x > n || n mod x <> 0 && go (x+1)
  in n > 1 && go 2

let rec primes b e =
  if b > e then [] else
    (if is_prime b then [b] else []) @ primes (b+1) e
