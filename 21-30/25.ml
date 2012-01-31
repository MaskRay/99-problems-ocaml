let rec rnd_permu xs =
  let a = Array.of_list xs in
  for i = 2 to Array.length a do
    let j = Random.int i in
    let t = a.(i-1) in
    a.(i-1) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a
