(* ex1 *)
infix at;
fun l at idx = List.hd(List.drop(l, idx));

(* ex2 *)
fun enumerate l = foldl (fn (x, y) => y @ [(List.length y, x)]) [] l;

(* ex3 *)
fun reverse l = foldl op:: [] l;

(* ex4 *)
fun flatten l = foldr op@ [] l;

(* ex5 *)
(* explanation for me to remember: the use of map is quite obvious, but how to
operate only on the element that satisfies the predictor is not. In order to
operate only only on these elements, for each element x in the list I create a
list [x, f(x)] if x does not satisfy the predictor or [f(x)] if it is.
then I take the head of that list. that does the trick, since the head is always
the result we want, by design. *)
fun applyif f pred l = List.map (fn x => List.hd ((List.filter (fn x => not (pred x)) [x]) @ [f x])) l;

(* ex6 *)
fun slice l (s, e) = List.take(List.drop(l, s), e - s);

(* ex7 *)
fun allholds preds l = List.filter (fn x => List.all (fn pred => pred x) preds) l;
