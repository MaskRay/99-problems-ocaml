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

let goldbach_list b e =
  let rec go b e =
    if b > e then [] else
      (b, goldbach b) :: go (b+2) e
  in go (b+b mod 2) e

let goldbach_list' b e bnd = List.filter (fun (_,(a,b)) -> a > bnd && b > bnd) (goldbach_list b e)
