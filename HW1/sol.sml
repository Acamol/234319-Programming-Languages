(** ex1 **)

(* finds if the parentheses in the string s are balanced *)
fun balance_aux (s, num_open, num_close, i) =
    if num_close > num_open then
        false
    else if i = size(s) then
        if num_open = num_close then
            true
        else
            false
    else if String.sub(s, i) = #"(" then
        balance_aux(s, num_open + 1, num_close, i + 1)
    else if String.sub(s, i) = #")" then
        balance_aux(s, num_open, num_close + 1, i + 1)
    else
        balance_aux(s, num_open, num_close, i + 1)
    ;

fun balance s =
    balance_aux(s, 0, 0, 0)
    ;


(** ex2 **)

(* converts a char that represents a number to int *)
fun charToNum c =
    ord c - ord #"0"
    ;

(* converts a string that represent a non-negative integer to integer *)
fun atoi_aux (s, i, res) =
    if i = size(s) then
        res
    else
        atoi_aux(s, i + 1, res * 10 + charToNum (String.sub(s, i)))
    ;

fun atoi s =
    atoi_aux (s, 0, 0)
    ;


(** ex3 **)

(* reverses the string s *)
fun reverseString_aux (s, size) =
    if size = 0 then
        s
    else if size = 1 then
        str(String.sub(s, 0))
    else
        str(String.sub(s, size - 1)) ^ reverseString_aux(s, size - 1)
    ;

fun reverseString s =
    reverseString_aux(s, size(s));


(** ex4 **)

(* pretty self-explantory function name *)
fun avoidNegativeGCD (a, b, c) =
    if a >= 0 then
        (a, b, c)
    else
        (~a, ~b, ~c)
    ;

(* extended eulcidean algorithm *)
fun extended_aux (oldr, r, olds, s, oldt, t) =
    if r = 0 then
        avoidNegativeGCD (oldr, olds, oldt)
    else
        extended_aux (r, oldr - oldr div r * r, s, olds - oldr div r * s, t, oldt - oldr div r * t)
    ;

fun extended (a, b) =
    extended_aux(a, b, 1, 0, 0, 1)
    ;
