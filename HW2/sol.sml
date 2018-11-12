
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
    fun partialSum s i idx_end =
        if i = idx_end then
            0
        else
            if ord (String.sub(s, i)) <= ord #"Z" then
                (1 + ord (String.sub(s, i)) - ord #"A") + partialSum s (i + 1) idx_end
            else
                (1 + ord (String.sub(s, i)) - ord #"a") + partialSum s (i + 1) idx_end
    ;

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
fun sig1 x y f =
    f(x, f(x, y))
;

fun sig2 (x,y) f =
    String.sub(f y, x * floor(y)) = #"a"
;

fun sig3 f x y z =
    f x y;

fun sig4 x y z =
    fn w => w * z
;

fun sig5 f x g =
    g(f(x), f(x))
;

fun sig6 () =
    fn () => 5
;

fun sig7 f l =
    fn l' => f(l'@l)
;
