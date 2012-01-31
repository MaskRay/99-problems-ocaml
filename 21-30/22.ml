let rec range a b =
  let rec go a b = if a > b then [] else a :: go (a+1) b
  in go (min a b) (max a b)
