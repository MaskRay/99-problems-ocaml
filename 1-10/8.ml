let rec nub = function
  | a :: (b :: _ as xs) -> if a = b then nub xs else a :: nub xs
  | xs -> xs
