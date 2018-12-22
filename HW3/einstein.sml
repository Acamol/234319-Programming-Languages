(* Aviad Gafni aviadgafni@campus.technion.ac.il 301836540 *)
(* Omri Zohar 204626261 ry@campus.technion.ac.il *)

datatype characteristic = Char of string list;
datatype puzzle = Puzzle of characteristic list * (db -> db)
    withtype db = string list list;
datatype connection = Connection of string list;

(* ex1 *)
local
    fun create_empty_char_list 0 = []
        | create_empty_char_list n = (create_empty_char_list (n - 1)) @ [Char []];
in
    fun riddle n = Puzzle (create_empty_char_list n, fn l => l);
end;

(* ex2 *)
local
    fun foldr_fun (nil, _)  = []
    |   foldr_fun (cs, nil) =  (map (fn x => [x]) cs)
    |   foldr_fun (c::cs, listOfLists) =
        (map (fn x => c :: x) listOfLists) @ (foldr_fun (cs, listOfLists))
    ;
in
    fun product charList =
    let
        val work = map (fn (Char x) => x) charList
    in
        foldr foldr_fun [] work
    end
end;

(* ex3 *)
local
    exception Fail;

    fun is_exists _ nil = false
    |   is_exists (element : string) (l'::l) =
          if l' = "" then
            false
          else
            (l' = element) orelse (is_exists element l)
        ;

    fun aux nil nil = []
    |   aux nil _   = raise Fail
    |   aux _ nil   = raise Fail
    |   aux (char::charList) (c::con) =
        let
            val Char char = char
        in
            if c = "" orelse (is_exists c char) then
                char :: (aux charList con)
            else
                (c :: char) :: (aux charList con)
        end
in
    fun update charList (Connection con) = map (fn x => Char x) (aux charList con)
end;

(* ex4 *)
local
	fun check_intersection (nil, nil) = false
	| check_intersection (filter, ls) =
        if ((size(hd(filter)) = 0) orelse (hd(filter) <> hd(ls))) then
            check_intersection(tl(filter), tl(ls))
		else
            true
    ;

	(*return true if string list in places of filter which have value, ls have the same value else false*)
	fun filterStrLLTrue ([]: string list, [] : string list) = true
	| filterStrLLTrue (filter: string list, ls : string list) =
        if ((size(hd(filter)) = 0) orelse (hd(filter) = hd(ls))) then
            filterStrLLTrue(tl(filter), tl(ls))
	    else
            false
    ;

	(* updates the filter func based on connection true or false *)
	fun update_fun (f: db->db) (Connection c) isTrue =
        if (isTrue = true) then
            (fn (strListList:db) =>  foldr (fn (strList, strLLOut) =>
            if (check_intersection(c, strList) = false orelse filterStrLLTrue(c, strList)) then
                strList::strLLOut
            else strLLOut) [] (f(strListList)))
		else (fn (strListList:db) =>
        foldr (fn (strList, strLLOut) =>
            if (check_intersection(c, strList) andalso filterStrLLTrue(c, strList)) then
                strLLOut else strList::strLLOut) [] (f(strListList)))
    ;

	(* adds a connection and update filter func *)
	fun add_connection (Puzzle pz) c isTrue =
        (Puzzle((update (#1pz) c), (update_fun (#2pz) c isTrue)));
in
	fun trueConnection  p c = add_connection p c true;
	fun falseConnection p c = add_connection p c false;
end;

fun solve (Puzzle pz) =
let
	(* check if lists intersect in any way *)
	fun check_intersection ([]: string list, [] : string list) = false
	| check_intersection (filter: string list, ls : string list) =
        if ((size(List.hd(filter)) = 0) orelse (List.hd(filter) <> List.hd(ls))) then
            (check_intersection(List.tl(filter), List.tl(ls)))
		else
            true
    ;

	fun flatten l = (foldl (fn (x, xs) => xs@x ) [] l);

	fun count (l, x) = let
		fun count_aux (nil , _, sum) = sum
		|   count_aux (l, x, sum) =
            if (x = hd(l)) then
                count_aux(tl(l), x, sum + 1)
            else
                count_aux(tl(l),x,sum);
in
		count_aux(l, x, 0)
end;

fun one_intersection (searchL : string list, ones: string list list) =
    List.exists (fn (oneL) => check_intersection(oneL, searchL)) ones;

fun filter_one_appearence ls =
    (foldr (fn (x,outL) => if (count(flatten ls,List.nth(x,0)) = 1) then x::outL else outL) [] ls);

fun remove_redundant (ls :db, ones: db) =
	if (length(ones) > 0) then
		(foldr (fn (x, ls2) => if (List.exists (fn (a) => a = x) ones) then x::ls2 else if (one_intersection(x,ones)) then ls2 else x::ls2) [] ls)
	else ls;

fun solve_aux(ls : string list list) =
    if (remove_redundant(ls, filter_one_appearence(ls)) = ls) then
        ls
    else
        solve_aux(remove_redundant(ls, filter_one_appearence(ls)));

in
    solve_aux((#2pz)(product (#1pz)))
end;
