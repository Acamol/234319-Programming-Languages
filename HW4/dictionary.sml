infix 2 --;
fun key -- value = (key, value);

datatype ('a, 'b) dictionary = dict of ('a * 'b) list
exception ItemIsNotPresent;

(* return all keys *)
fun keys (dict d)  =
    map (fn (key, _) => key)  d
;

(* return all values *)
fun values (dict d) =
    map (fn (_, value) => value) d
;

(*
    evaluate the key-value pair at the head and check if the key matches.
    if it does, return its value, otherwise continute with the tail.
    in case the function got to nil (end of dictionary), raise ItemIsNotPresent.
*)
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

(*
    defines the access operator. i.e. to get a value of a key named 2 in a
    dictionary d, you'd use it like so:
        d |> 2;
*)
infix |>;
fun d |> key = access d key;

(*
    put the pair key-value in the dictionary. if key is already associated with
    a value, its value would change to the new value.
*)
fun put (dict d) (key, value) =
    if (List.exists (fn (x, _) => x = key) d) then
        dict (map (fn (x, y) => if x = key then (key, value) else (x,y)) d)
    else
        dict ( d @ [(key, value)])
;

(*
    defines the insert operator. i.e. to insert the pair key-value to a
    dictionary d, you'd use it like so:
        d |< (key, value);
*)
infix |<;
fun d |< (key, value) = put d (key, value);

(*
    remove the pair (key, _) from a dictionary.
    in order to do so, first it searches if the key exists. if not, it raises
    ItemIsNotPresent. otherwise, it filters the elements whose keys are key.
    since in a dictionary all keys are unique only one pair is removed, as
    should be.
*)
fun remove (dict d) key =
    if (List.exists (fn (x, _) => x = key) d) then
        dict (List.filter (fn (x, y) => x <> key) d)
    else
        raise ItemIsNotPresent
;

(*
    defines the remove operator. i.e. to remove the a pair associated with a key
    "key" in a dictionary d, you'd use it like so:
        d |\ key;
*)
infix |\;
fun d |\ key = remove d key;

(*
    filter the list according to a given predicator.
*)
fun keep pred (dict d) =
    dict (List.filter pred d)
;

(*
    defines the filter operator. i.e. to filter all pairs in a dictionary d,
    which fulfill some predicator "pred", you'd use it like so:
        d |& pred
*)
infix |&;
fun d |& pred = keep pred d;

(*
    create new dictionary in which all pairs will be grouped according to a
    given function.
    for example:
        dict nil |< ("Aviad", "Gafni") -- "Na'ama" |< ("Aviad", "Kisos") -- "Tel Aviv"
        |< ("Jeff", "Buckley") -- "Anaheim" |< ("Tim", "Buckley") -- "Washington";
        groupby (fn (key, value) => #1 key) it;

    produces:
        val it = dict [
        ("Tim",
            dict [(("Tim","Buckley"),"Washington")]),
        ("Jeff",
            dict [(("Jeff","Buckley"),"Anaheim")]),
        ("Aviad",
            dict [(("Aviad","Kisos"),"Tel Aviv"),
                  (("Aviad","Gafni"),"Na'ama")])]
    : (string,(string * string,string) dictionary) dictionary

    i.e. group the pairs of fullname-hometown by their first name.
*)
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
