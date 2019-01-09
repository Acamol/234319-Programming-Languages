Control.Print.printDepth := 1000;
(* Do not touch these functions, you have been warned *)

fun count nil t = 0
|	count (x::xs) t = 	if t = x then
							1 + (count xs t)
						else
							(count xs t);

infix <<=;
fun l1  <<= l2 = List.all (fn x => (count l1 x) = (count l2 x)) l1;

infix listeq;
fun l1 listeq l2 = l1 <<= l2 andalso l2 <<= l1;

infix ===;
fun (dict d1) === (dict d2) = d1 listeq d2;

fun count2 nil _ = 0
|	count2 ((s,d)::xs) (s',d') =	if (s = s') andalso (d === d') then
										1 + (count2 xs (s', d'))
									else (count2 xs (s', d'));

infix <<<=;
fun l1 <<<= l2 = List.all (fn x => (count2 l1 x) = (count2 l2 x)) l1;

infix ====;
fun (dict d1) ==== (dict d2) = d1 <<<= d2 andalso d2 <<<= d1;

fun count3 nil _ = 0
|	count3 ((s, d)::xs) (s', d') =	if (s = s') andalso (d ==== d') then
										1 + (count3 xs (s', d'))
									else (count3 xs (s', d'));

infix <<<<=;
fun l1 <<<<= l2 = List.all (fn x => (count3 l1 x) = (count3 l2 x)) l1;

infix =====;
fun (dict d1) ===== (dict d2) = d1 <<<<= d2 andalso d2 <<<<= d1;

fun dictsize (dict d) = length d;
exception testExep;

(* ------ keys test ------ *)
print "------ keys test ------";

val test1 = keys (dict [1 -- 2, 4 -- 5, 5 -- 1]) listeq [1, 4, 5];
val test2 = keys (dict nil) listeq nil;
val test3 = keys (dict ["s" -- "1", "t" -- "2", "f" -- "3", "a" -- "4", "o" -- "5"]) listeq ["s", "a", "f", "o", "t"];
val test4 = keys (dict [3 -- 4 -- 5]) listeq [(3, 4)];
val test5 = keys (dict [1 -- 2 -- 3, 2 -- 3 -- 4, 5 -- 6 -- 7, 8 -- 8 -- 8]) listeq [1 -- 2, 2 -- 3, 5 -- 6, 8 -- 8];

(* ------ values test ------ *)
print "------ values test ------";

val test1 = values (dict [1 -- 2, 4 -- 5, 5 -- 1]) listeq [1, 5, 2];
val test2 = values (dict nil) listeq nil;
val test3 = values (dict ["s" -- "1", "t" -- "2", "f" -- "3", "a" -- "4", "o" -- "5"]) listeq ["1", "2", "3", "4", "5"];
val test4 = values (dict [3 -- 4 -- 5]) listeq [5];
val test5 = values (dict [1 -- 2 -- 3, 2 -- 3 -- 4, 5 -- 6 -- 7, 8 -- 8 -- 8]) listeq [3, 4, 7, 8];
val test6 = values (dict [1 -- 2, 2 -- 2, 3 -- 2, 4 -- 3]) listeq [2, 3, 2, 2];
val test7 = values (dict ["a" -- "i", "b" -- "i", "c" -- "i"]) listeq ["i", "i", "i"];
val test8 = values (dict [1 -- (2 -- 3), 4 -- (5 -- 6), 6 -- (2 -- 6), 7 -- (2 -- 3)]) listeq [2 -- 3, 5 -- 6, 2 -- 6, 2 -- 3];

(* ------ access test ------ *)
print "------ access test ------";
val test1 = (dict[1 -- 2] |> 1) = 2;
val test2 = (dict[1 -- 2, 2 -- 3, 3 -- 4] |> 1) = 2;
val test3 = (dict[1 -- 2, 2 -- 3, 3 -- 4] |> 2) = 3;
val test4 = (dict[1 -- 2, 2 -- 3, 3 -- 4] |> 3) = 4;
val test5 = (dict[1 -- 2 -- 3, 2 -- 3 -- 1] |> 1 -- 2) = 3; 
val test6 = (dict[1 -- 2 -- 3, 2 -- 3 -- 1] |> 2 -- 3) = 1; 
val test7 = (dict["ML" -- "good", "CPP" -- "bad"] |> "ML") = "good";
val test8 = (dict nil |> 4) handle ItemIsNotPresent => true;
val test9 = (dict nil |> nil) handle ItemIsNotPresent => true;
val test10 = (dict ["ML" -- "good", "CPP" -- "bad"] |> "ml") = "---" handle ItemIsNotPresent => true;
val test11 = (dict[1 -- 2, 2 -- 3, 3 -- 4] |> 4) = ~1 handle ItemIsNotPresent => true;

(* ------ put test ------ *)
print "------ put test ------";

val test1 = dict nil |< 3 -- 4 === dict [3 -- 4];
val test2 = dict[3 -- 4] |< 4 -- 5 === dict[3 -- 4, 4 -- 5];
val test3 = dict[3 -- 4] |< 3 -- 4 === dict[3 -- 4];
val test4 = dict[3 -- 4] |< 3 -- 5 === dict[3 -- 5];
val test5 = (dict["safot" -- "good"] |< "cpp" -- "bad" |> "safot") = "good";
val test6 = (dict nil |< 1 -- 2 |< 2 -- 3 |> 2) = 3;
val test6 = (dict nil |< 3 -- 4 |< 3 -- 5 |> 3) = 5;
val test7 = (dict ["1" -- "one"] |< "2" -- "two" |< "1" -- "ONE" |< "2" -- "TWO") === dict ["1" -- "ONE", "2" -- "TWO"];
val test8 = let val t = (dict [1 -- op+, 2 --op-]) |< 1 -- op* in true end;

(* ------ remove test ------ *)
print "------ remove test ------";

val test1 = ((dict nil |\ 1) === dict [2 -- 3]) handle ItemIsNotPresent => true;
val test2 = (dict nil |\ nil) === dict[nil -- 3] handle ItemIsNotPresent => true;
val test3 = (dict[1 -- 2] |\ 1) === dict nil;
val test4 = (dict[1 -- 2] |\ 2) === dict [2 -- 3] handle ItemIsNotPresent => true;
val test5 = (dict["safot" -- "good", "cpp" -- "bad"] |\ "safot") === dict["cpp" -- "bad"];
val test6 = let val t = (dict [1 -- op+, 2 --op-] |\ 2) in true end;

(* ------ keep test ------ *)
print "------ keep test ------";
val test1 = dict nil |& (fn (x, y) => false) === dict nil;
val test2 = dict nil |& (fn (x, y) => true) === dict nil;
val test3 = dict[1 -- 2, 2 -- 3, 3 -- 4] |& (fn (x, y) => true) === dict[1 -- 2, 2 -- 3, 3 -- 4];
val test4 = dict[1 -- 2, 2 -- 3, 3 -- 4] |& (fn (x, y) => false) === dict nil;
val test5 = dict[1 -- 2, 2 -- 3, 3 -- 4, 4 -- 5] |& (fn (x, _) => x mod 2 = 0) === dict[2 -- 3, 4 -- 5];
val test6 = dict[1 -- "t", 2 -- "t", 3 -- "th", 4 -- "f"] |& (fn (x, _) => x mod 2 = 0) === dict[2 -- "t", 4 -- "f"];
val test7 = dict[1 -- 2, 2 -- 3, 3 -- 4, 4 -- 5] |& (fn (_, y) => y mod 2 = 0) === dict[1 -- 2, 3 --4];
val test8 = dict[1 -- 2, 2 -- 3, 3 -- 4, 4 -- 5] |& (fn (x, y) => (x+y) mod 2 = 0) === dict nil;
val test9 = dict[1 -- 3, 2 -- 5, 4 -- 7, 32 -- 2] |& (fn (x, y) => (x+y) mod 2 = 0) === dict[1 -- 3, 32 --2];
val test10 = dict[1 -- 3, 2 -- 5, 4 -- 7, 32 -- 2] |& (fn (x, y) => x mod 2 = 0 andalso y mod 2 = 0) === dict[32 -- 2];
val test11 = keys (dict ["p" -- op+, "m" -- op-, "l" -- op*] |& (fn (_, y) => y(2, 2) = 4)) listeq ["p", "l"];
val test12 = keys (dict["h" -- (fn (_) => true), "p" -- (fn (_) => false)] |& (fn (x, y) => y x)) listeq ["h"];

(* ------ groupby test ------ *)
print "------ groupby test ------";
val d = dict ["Michal" -- 9, "Gal" -- 6, "Yaron" -- 19, "Tal" -- 21, "Ben" -- 17, "Daniel" -- 15];
val test1 = groupby (fn (_, v) => Int.toString (v div 10 * 10) ^ "+") d ====
			dict[	"0+" -- (dict["Michal" -- 9, "Gal" -- 6]),
					"10+" -- (dict["Ben" -- 17, "Yaron" -- 19,  "Daniel" -- 15]),
					"20+" -- (dict["Tal" -- 21])]; 
val test2 = groupby (fn _ => 5) d ==== dict[5 -- d];
val test3 = groupby (fn (v, _) => String.isSubstring "l" v) d ====
			dict[	true -- dict["Michal" -- 9, "Gal" -- 6, "Daniel" -- 15, "Tal" -- 21],
					false -- dict["Ben" -- 17, "Yaron" -- 19]];
val test4 = groupby (fn (s, n) => n - (String.size(s))) d ====
			dict[	3 -- dict["Michal" -- 9, "Gal" -- 6],
					14 -- dict[ "Ben" -- 17, "Yaron" -- 19],
					9 -- dict["Daniel" -- 15],
					18 -- dict["Tal" -- 21]];
val test5 = groupby (fn (_, v) => Int.toString (v div 5 * 10) ^ "+") d
			====
			dict[
				"30+" -- dict["Yaron" -- 19, "Daniel" -- 15, "Ben" -- 17],
				"10+" -- dict["Michal" -- 9, "Gal" -- 6],
				"40+" -- dict["Tal" -- 21]
			];

(* if this is the only test you fail, your'e basically fucked. good luck with debugging. *)
val testus_maximus = 	groupby (fn (x, y) => 2 * String.size(x) - (dictsize y)) 
						(groupby (fn (_, v) => Int.toString (v div 10 * 10) ^ "+") 
						(d |< "Cantor" -- 31 |< "Schroder" -- 32 |< "Bernstein" -- 33)
						)
						=====
						dict[	
							2 -- dict[	"0+" -- dict[
														"Michal" -- 9,
														"Gal" -- 6
													]
									 ],
							3 -- dict[	
										"10+" -- dict[
														"Yaron" -- 19,
														"Ben" -- 17,
														"Daniel" -- 15
													 ],
										"30+" -- dict[
														"Cantor" -- 31,
														"Schroder" -- 32,
														"Bernstein" -- 33
													 ]
									 ],
							5 -- dict[
										"20+" -- dict[
														"Tal" -- 21
													 ]
									 ]
							] ;

print "even if all tests passed, don't forget to run valgrind!"