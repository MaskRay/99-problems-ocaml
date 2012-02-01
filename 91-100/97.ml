let nodes = 4*9*9*9+4*9*9+1
let senti = 4*9*9

class sudoku (a : char array array) =
object (self)
  val mutable ll = Array.create nodes 0
  val mutable rr = Array.create nodes 0
  val mutable uu = Array.create nodes 0
  val mutable dd = Array.create nodes 0
  val mutable col = Array.create nodes 0
  val mutable size = Array.create nodes 0
  val mutable mean = Array.create nodes (0,0,0)

  method add x l r c =
    ll.(x) <- l; rr.(x) <- r;
    uu.(x) <- uu.(c); dd.(uu.(x)) <- x;
    dd.(x) <- c; uu.(c) <- x;
    col.(x) <- c; size.(c) <- size.(c) + 1

  method cover c =
    ll.(rr.(c)) <- ll.(c); rr.(ll.(c)) <- rr.(c);
    let rec go1 i =
      if i <> c then begin
	let rec go2 j =
	  if j <> i then begin
	    uu.(dd.(j)) <- uu.(j);
	    dd.(uu.(j)) <- dd.(j);
	    go2 rr.(j)
	  end in
	go2 rr.(i);
	go1 dd.(i);
      end in
    go1 dd.(c)

  method uncover c =
    let rec go1 i =
      if i <> c then begin
	let rec go2 j =
	  if j <> i then begin
	    uu.(dd.(j)) <- j;
	    dd.(uu.(j)) <- j;
	    go2 ll.(j)
	  end in
	go2 ll.(i);
	go1 uu.(i);
      end in
    go1 uu.(c);
    ll.(rr.(c)) <- c; rr.(ll.(c)) <- c

  method dlx =
    if rr.(senti) = senti then raise Exit
    else
      let rec go0 c i =
	if i = senti then c
	else if size.(i) < size.(c) then go0 i rr.(i)
	else go0 c rr.(i) in
      let c = go0 rr.(senti) rr.(senti) in
      self#cover c;
      let rec go1 i =
	if i <> c then begin
	  let rec go2 j =
	    if j <> i then begin
	      self#cover col.(j);
	      go2 rr.(j)
	    end in
	  go2 rr.(i);
	  let r', c', k' = mean.(i) in
	  a.(r').(c') <- Char.chr (k' + Char.code '1');
	  self#dlx;
	  let rec go2 j =
	    if j <> i then begin
	      self#uncover col.(j);
	      go2 ll.(j)
	    end in
	  go2 ll.(i);
	  go1 dd.(i);
	end in
      go1 dd.(c);
      self#uncover c;

  initializer
    for i = 0 to senti do
      ll.(i) <- i-1; rr.(i) <- i+1;
      uu.(i) <- i; dd.(i) <- i
    done;
    ll.(0) <- senti; rr.(senti) <- 0;
    let v = ref (senti+1) in
    let add_line i j k =
      self#add !v (!v+3) (!v+1) (9*i+j);
      mean.(!v) <- (i,j,k); v := !v+1;
      self#add !v (!v-1) (!v+1) (9*9+9*i+k);
      mean.(!v) <- (i,j,k); v := !v+1;
      self#add !v (!v-1) (!v+1) (9*9*2+9*j+k);
      mean.(!v) <- (i,j,k); v := !v+1;
      self#add !v (!v-1) (!v-3) (9*9*3+(i/3*3+j/3)*9+k);
      mean.(!v) <- (i,j,k); v := !v+1
    in
    for i = 0 to 9-1 do
      for j = 0 to 9-1 do
    	if '1' <= a.(i).(j) && a.(i).(j) <= '9' then
    	  add_line i j (Char.code a.(i).(j) - Char.code '1')
    	else
    	  for k = 0 to 9-1 do
    	    add_line i j k
    	  done
      done
    done;
    try self#dlx with
      Exit ->
	for i = 0 to 9-1 do
	  for j = 0 to 9-1 do
	    print_char a.(i).(j)
	  done;
	  print_char '\n'
	done
end

let a = Array.create_matrix 9 9 'x'

let rec fill = function
  | (9, _) -> new sudoku a
  | (r, 9) -> fill (r+1, 0)
  | (r, c) ->
      while not ('0' <= a.(r).(c) && a.(r).(c) <= '9' || a.(r).(c) = '.') do
	Scanf.scanf "%c" (fun v -> a.(r).(c) <- v)
      done;
      fill (r, c+1);;

fill (0, 0)
