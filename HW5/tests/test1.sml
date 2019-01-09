(*
    Tests for 236319 2019A HW5 Q1
    All boolean tests should return true
*)

(* Remove the following line in case you want to see all the signatures*)

Control.Print.out := {say=fn _=>(), flush=fn()=>()}; 

(* Testing function: build4 *)

val _ = print("\nTesting function: build4\n");


(* Additional functions *)

val _ = print("\n~~~ Additional functions ~~~\n");

fun printTestOutput string result =
  if result then print ("### V V V - PASSED - " ^ string ^ " ###\n\n")
  else print ("### X X X - FAILED - " ^ string ^ " ###\n\n");
  
fun CHECK string result = printTestOutput string result;


(* Testing function: build4 *)

val _ = print("\nTesting function: build4\n");
val _ = print("\n~~~ Testing function: build4 - subtest 1 ~~~\n");

val (x++one++y++two++NIL) = build4 ("x",1,"y",2);
CHECK ("build4 test 1 subtest 1") (x = "x");
CHECK ("build4 test 1 subtest 2") (one = 1);
CHECK ("build4 test 1 subtest 3") (y = "y");
CHECK ("build4 test 1 subtest 4") (two = 2);

val _ = print("\n~~~ Testing function: build4 - subtest 2 ~~~\n");

val (x++one++y++two++NIL) = build4 (#"a",1,#"b",2);
CHECK ("build4 test 2 subtest 1") (x = #"a");
CHECK ("build4 test 2 subtest 2") (one = 1);
CHECK ("build4 test 2 subtest 3") (y = #"b");
CHECK ("build4 test 2 subtest 4") (two = 2);

val _ = print("\n~~~ Testing function: build4 - subtest 3 ~~~\n");

val (x++one++y++two++NIL) = build4 (1,2,3,4);
CHECK ("build4 test 3 subtest 1") (x = 1);
CHECK ("build4 test 3 subtest 2") (one = 2);
CHECK ("build4 test 3 subtest 3") (y = 3);
CHECK ("build4 test 3 subtest 4") (two = 4);

val _ = print("\n~~~ Testing function: build4 - subtest 4 ~~~\n");

(* Verifies that you did not override :: *)

val (x++one++y++two++NIL) = build4 (5::6::nil,2,6::5::nil,4);
CHECK ("build4 test 4 subtest 1") (x = [5,6]);
CHECK ("build4 test 4 subtest 2") (one = 2);
CHECK ("build4 test 4 subtest 3") (y = [6,5]);
CHECK ("build4 test 4 subtest 4") (two = 4);

val _ = print("\n~~~ Testing function: build4 - subtest 5 ~~~\n");

val (x++one++y++two++NIL) = build4 ("safot",236319,"Winter",2019);
CHECK ("build4 test 5 subtest 1") (x = "safot");
CHECK ("build4 test 5 subtest 2") (one = 236319);
CHECK ("build4 test 5 subtest 3") (y = "Winter");
CHECK ("build4 test 5 subtest 4") (two = 2019);

val _ = print("\n~~~ Testing function: build4 - subtest 6 ~~~\n");

val (x++one++y++two++NIL) = build4 ([1,2],[],[5,6],[#"a",#"b"]);
CHECK ("build4 test 6 subtest 1") (x = [1,2]);
CHECK ("build4 test 6 subtest 2") (one = []);
CHECK ("build4 test 6 subtest 3") (y = [5,6]);
CHECK ("build4 test 6 subtest 4") (two = [#"a",#"b"]);

val _ = print("\n~~~ Testing function: build4 - subtest 7 ~~~\n");

val (x++one++y++two++NIL) = build4 ([1,2],false,[3,4],true);
CHECK ("build4 test 7 subtest 1") (x = [1,2]);
CHECK ("build4 test 7 subtest 2") (one = false);
CHECK ("build4 test 7 subtest 3") (y = [3,4]);
CHECK ("build4 test 7 subtest 4") (two = true);



(* Testing function: unzip *)

val _ = print("\n~~~ Testing function: unzip ~~~\n");

val newList = unzip(build4 ("x",1,"y",2));
CHECK ("unzip test 1") (newList = (["x","y"],[1,2]));

val newList = unzip(build4 (3,1,4,2));
CHECK ("unzip test 2") (newList = ([3,4],[1,2]));

val newList = unzip(build4 (#"3",1,#"4",2));
CHECK ("unzip test 3") (newList = ([#"3",#"4"],[1,2]));

val newList = unzip(build4 (3,#"1",4,#"2"));
CHECK ("unzip test 4") (newList = ([3,4],[#"1",#"2"]));

val newList = unzip(build4 ([1,2],false,[3,4],true));
CHECK ("unzip test 5") (newList = ([[1,2],[3,4]],[false,true]));

val newList = unzip(build4 ([1,2],[],[5,6],[#"a",#"b"]));
CHECK ("unzip test 6") (newList = ([[1,2],[5,6]],[[],[#"a",#"b"]]));

val newList = unzip(build4 ("safot",236319,"Winter",2019));
CHECK ("unzip test 7") (newList = (["safot","Winter"],[236319,2019]));

val newList = unzip(build4 (5::6::nil,2,6::5::nil,4));
CHECK ("unzip test 8") (newList = ([[5,6],[6,5]],[2,4]));

val newList = unzip(build4 ("x","y","w","z"));
CHECK ("unzip test 9") (newList = (["x","w"],["y","z"]));

val newList = unzip(build4 ([[[false]],[[true,false]]],"a",[[[true]],[[false]]],"b"));
CHECK ("unzip test 10") (newList = ([[[[false]],[[true,false]]],[[[true]],[[false]]]],["a","b"]));

(* Testing function: zip *)

val _ = print("\n~~~ Testing function: zip ~~~\n");

val newList = zip(unzip(build4 ("x",1,"y",2)));
CHECK ("zip test 1") (newList = (build4 ("x",1,"y",2)));

val newList = zip(unzip(build4 (3,1,4,2)));
CHECK ("zip test 2") (newList = (build4 (3,1,4,2)));

val newList = zip(unzip(build4 (#"3",1,#"4",2)));
CHECK ("zip test 3") (newList = (build4 (#"3",1,#"4",2)));

val newList = zip(unzip(build4 (3,#"1",4,#"2")));
CHECK ("zip test 4") (newList = (build4 (3,#"1",4,#"2")));

val newList = zip(unzip(build4 ([1,2],false,[3,4],true)));
CHECK ("zip test 5") (newList = (build4 ([1,2],false,[3,4],true)));

val newList = zip(unzip(build4 ([1,2],[],[5,6],[#"a",#"b"])));
CHECK ("zip test 6") (newList = (build4 ([1,2],[],[5,6],[#"a",#"b"])));

val newList = zip(unzip(build4 ("safot",236319,"Winter",2019)));
CHECK ("zip test 7") (newList = (build4 ("safot",236319,"Winter",2019)));

val newList = zip(unzip(build4 (5::6::nil,2,6::5::nil,4)));
CHECK ("zip test 8") (newList = (build4 (5::6::nil,2,6::5::nil,4)));

val newList = zip(unzip(build4 ("x","y","w","z")));
CHECK ("zip test 9") (newList = (build4 ("x","y","w","z")));

val newList = zip(unzip(build4 ([[[false]],[[true,false]]],"a",[[[true]],[[false]]],"b")));
CHECK ("zip test 10") (newList = (build4 ([[[false]],[[true,false]]],"a",[[[true]],[[false]]],"b")));
