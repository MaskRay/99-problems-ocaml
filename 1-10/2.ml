let rec last2 = function
  | [x;y] -> x
  | _::xs -> last2 xs
