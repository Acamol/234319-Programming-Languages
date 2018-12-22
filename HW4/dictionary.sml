infix 2 --;
fun key -- value = (key, value);

datatype ('a, 'b) dictionary = dict of ('a * 'b) list
exception ItemIsNotPresent;

fun keys (dict d)  =
    map (fn (key, _) => key)  d
;

fun values (dict d) =
    map (fn (_, value) => value)
;

fun access (dict d) key =
    let
        val pair = List.hd d handle Empty => raise ItemIsNotPresent
    in
        if (#1 pair) = key then
            #2 pair
        else
            access (dict (List.tl(d))) key
    end
;

infix |>;
fun d |> key = access d key;

fun put (dict d) (key, value) =
    if (List.exists (fn (x, _) => x = key) d) then
        dict (map (fn (x, y) => if x = key then (key, value) else (x,y)) d)
    else
        dict ( d @ [(key, value)])
;

infix |<;
fun d |< (key, value) = put d (key, value);

fun remove (dict d) key =
    if (List.exists (fn (x, _) => x = key) d) then
        dict (List.filter (fn (x, y) => x <> key) d)
    else
        raise ItemIsNotPresent
;

infix |\;
fun d |\ key = remove d key;

fun keep pred (dict d) =
    dict (List.filter pred d)
;

infix |&;
fun d |& pred = keep pred d;

local
    fun groupby_aux f nil = dict nil
    |   groupby_aux f (head::d) =
        let
            val curr_dict = groupby_aux f d;
            val key = f head;
        in
            curr_dict |< key -- ((curr_dict |> key handle ItemIsNotPresent => dict nil) |< head)
        end
in
    fun groupby f (dict d) = groupby_aux f d;
end;

(*
dict nil |< "Michal" -- 9 |< "Gal" -- 6 |< "Yaron" -- 19;
it |< "Tal"-- 21 |< "Ben" -- 17 |< "Daniel" -- 15;
groupby (fn (_, v) => Int.toString (v div 10 * 10) ^ "+") it;
*)
