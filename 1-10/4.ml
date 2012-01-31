let rec length = function
  | [] -> 0
  | _::xs -> 1 + length xs
