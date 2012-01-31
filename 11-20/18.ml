let rec drop n = function
  | [] -> []
  | h::t as l -> if n <= 0 then l else drop (n-1) t

let rec take n = function
  | [] -> []
  | h::t -> if n <= 0 then [] else h :: take (n-1) t

let slice xs b e = take (e-b+1) (drop (b-1) xs)
