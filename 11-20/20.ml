let rec remove_at xs n = match xs with
  | [] -> []
  | h::t -> if n == 1 then t else h :: remove_at t (n-1)
