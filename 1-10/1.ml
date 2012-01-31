let rec last = function
  | [x] -> x
  | _::xs -> last xs
