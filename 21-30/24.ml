let rec rnd_select n l = if l = 0
  then []
  else begin
    let r = Random.int l in
    if r < n then
      l :: rnd_select (n-1) (l-1)
    else
      rnd_select n (l-1)
  end
