let rec rnd_select l = function
  | 0 -> []
  | n -> match l with
      | [] -> []
      | x::xs -> let r = Random.int (List.length l) in
		 if r < n then
		   x ::rnd_select xs (n-1)
		 else
		   rnd_select xs n
