let split xs n =
  let rec go acc n = function
    | [] -> List.rev acc, []
    | h::t as a -> if n == 0 then List.rev acc, a else go (h::acc) (n-1) t
  in go [] n xs

let rotate xs n =
  if xs = []
  then []
  else begin
    let len = List.length xs in
    let (a,b) = split xs (n mod len) in
    b @ a
  end
