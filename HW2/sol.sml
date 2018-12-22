(* Aviad Gafni 301836540 aviadgafni@campus.technion.ac.il Omri Zohar 204626261 ry@campus.technion.ac.il *)

(* ex1 *)
fun assertThat x f y =
    f y x = true
;

fun equalTo x y =
    x = y
;

fun doesnt f x =
    not (f x)
;

fun bothOf (f, g) x =
    f(x) andalso g(x)
;


(* ex2 *)

local
    (*
    calculates the index of a letter in the English alphabet,
    ignoring case
    *)
    fun letterValue c =
        if ord c <= ord #"Z" then
            (1 + ord c - ord #"A")
        else
            (1 + ord c - ord #"a")
    ;

    (*
    calculates the partial sum of string s,
    statring from 0 to idx_end (exluding)
    *)
    fun partialSum s i idx_end =
        if i = idx_end then
            0
        else
            letterValue (String.sub(s, i)) + partialSum s (i + 1) idx_end;
    ;

    (* creates the actual list by concatenating each partial sum *)
    fun createList s i =
        if i > size(s) then
            []
        else
            (partialSum s 0 i) :: (createList s (i + 1))
    ;
in
    fun prefixSum s =
        createList s 1
    ;
end;


(* ex3 *)

(* 'a -> 'b -> ('a * 'b -> 'b) -> 'b *)
fun sig1 x y f =
    f(x, f(x, y))
;

(* int * real (real -> string) -> bool *)
fun sig2 (x,y) f =
    String.sub(f y, x * floor(y)) = #"a"
;

(* ('a -> 'b -> c') -> 'a -> 'b -> 'd -> 'c *)
fun sig3 f x y z =
    f x y;

(* 'a -> 'b -> int -> int -> int *)
fun sig4 x y z =
    fn w => w * z
;

(* ('a -> 'b) -> 'a -> ('b * 'b -> c) -> c *)
fun sig5 f x g =
    g(f(x), f(x))
;

(* unit -> unit -> int *)
fun sig6 () =
    fn () => 5
;

(* ('a list -> 'b) -> 'a list-> 'a list -> 'b *)
fun sig7 f l =
    fn l' => f(l'@l)
;
