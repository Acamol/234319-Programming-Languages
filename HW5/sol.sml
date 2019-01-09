(* part 1 - heterolist *)

datatype ('a, 'b) heterolist = NIL | ++ of 'a * ('b, 'a) heterolist;
infixr 5 ++

fun build4 (a1, b1, a2, b2) = a1 ++ b1 ++ a2 ++ b2 ++ NIL;

(*val x1++x2++x3++x4++NIL = build4("x", 1, "y", 2);*)

local
    fun tail NIL       = raise Empty
    |   tail (x ++ xs) = xs
    ;

    (* create a list from elements in even indices in a hetrolist *)
    fun even NIL        = nil
    |   even (x ++ NIL) = x :: nil
    |   even (x ++ xs)  = x :: (even (tail xs))
    ;

    (* create a list from elements in odd indices in a hetrolist *)
    fun odd NIL       = nil
    |   odd (x ++ xs) = even xs;
in
    fun unzip (xs) = (even xs, odd xs);

    fun zip (nil, nil)         = NIL
    |   zip (nil, _)           = raise Empty
    |   zip (x :: xs, nil)     = x ++ NIL
    |   zip (x :: xs, y :: ys) = x ++ y ++ zip(xs, ys)
    ;
end;



(* part 2 - sequence *)
datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

exception EmptySeq;

fun head (Cons (x, _)) = x
|   head Nil           = raise EmptySeq;

fun tail (Cons (_, xf)) = xf()
|   tail Nil            = raise EmptySeq;

datatype direction = Back | Forward;
datatype 'a bseq =   bNil | bCons of 'a * (direction -> 'a bseq);

fun bHead (bCons (x, _)) = x
|   bHead bNil = raise EmptySeq;

fun bForward (bCons (_, xf)) = xf(Forward)
|   bForward bNil = raise EmptySeq;

fun bBack (bCons (_, xf)) = xf(Back)
|   bBack bNil = raise EmptySeq;

fun intbseq x =
    bCons (x,
           fn d =>
                if d = Back then
                    intbseq (x - 1)
                 else
                    intbseq (x + 1)
          )
;

fun bmap f bNil           = bNil
|   bmap f (bCons (x, xf)) = bCons (f(x), fn d => bmap f (xf(d)));


fun bfilter _ _ bNil = bNil
|   bfilter pred d (bCons (x, xf)) =
        if pred x then
            bCons (x, fn d => bfilter pred d (xf(d)))
        else
            bfilter pred d (xf(d))
;

fun seq2bseq (Cons (r, rf)) (Cons (n, nf)) =
    bCons (n,
           fn d =>
                if d = Forward then
                    seq2bseq (Cons (n, fn () => Cons (r, rf))) (nf())
                else
                    seq2bseq (rf()) (Cons (r, fn () => Cons (n, nf)))
          )
|   seq2bseq _ _ = bNil (* the assumption is that we only get infinite
                           sequences, so this pattern is just here to
                           avoid "match nonexhaustive" errors *)
;

local
    (* get the next n-th bCons in a bseq *)
    fun nextCons n (bCons (x, xf)) d =
        if n = 1 then
            xf(d)
        else
            nextCons (n-1) (xf(d)) d
    |   nextCons _ bNil _ = bNil
    ;
in
    fun bSeqJump (bCons (x, xf)) n =
        bCons (x,
               fn d =>
                    bSeqJump (nextCons n (bCons (x, xf)) d) n
              )
    |   bSeqJump bNil _ = bNil (* the assumption is that we only get infinite
                                  sequences, so this pattern is just here to
                                  avoid "match nonexhaustive" errors  *)
    ;
end;
