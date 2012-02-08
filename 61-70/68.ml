type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let rec preorder = function
  | Leaf -> ()
  | Branch (x,l,r) -> print_char x; preorder l; preorder r

let rec inorder = function
  | Leaf -> ()
  | Branch (x,l,r) -> inorder l; print_char x; inorder r

let build preorder inorder =
  let rec go l1 r1 l2 r2 =
    if l1 >= r1 then
      Leaf
    else begin
      let r = preorder.[l1] in
      let m2 = String.index_from inorder l2 r in
      Branch (r,
	      go (l1+1) (l1+1+m2-l2) l2 m2,
	      go (l1+1+m2-l2) r1 (m2+1) r2)
    end in
  let len = String.length preorder in
  go 0 len 0 len
