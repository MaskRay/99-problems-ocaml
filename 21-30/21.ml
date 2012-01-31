let rec insert_at x xs n = match xs with
  | [] -> []
  | h::t as l -> if n == 1 then x::l else h :: insert_at x t (n-1)
