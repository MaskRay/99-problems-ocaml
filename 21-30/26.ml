let combination k xs =
  let rec go k run acc = function
    | [] -> acc
    | h::t -> if k = 1
      then go k run ((h::run)::acc) t
      else go k run (go (k-1) (h::run) acc t) t
  in go k [] [] xs
