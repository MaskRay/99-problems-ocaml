let rec kth (x::xs) = function
  | 1 -> x
  | k -> kth xs (k-1)
