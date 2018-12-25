datatype S = NIL
           | STR of string
           | INT of int
           | CONS of S * S
           | EAGER of FUNC
           | NORMAL of FUNC
and FUNC =
             UNARY of (S -> S -> S)
           | BINARY of (S * S -> S -> S)
           | TRINARY of (S * S * S -> S -> S)
           ;

(* the basic "true" value representation. its counterpart is NIL. *)
val T = STR "T";

local
    exception FAIL of string
in
    fun CAR (CONS (car, cdr)) (env : S) = car
    |   CAR _ env                       =
            raise FAIL "CAR:: argument is not CONS"
    ;

    fun CDR (CONS (car, cdr)) (env : S) = cdr
    |   CDR _ env                       =
            raise FAIL "CDR:: argument is not CONS"
    ;
end;

fun NULL NIL (env : S) = T
|   NULL _ (env : S)   = NIL
;

fun INTEGER (INT _) (env : S) =  T
|   INTEGER _ (env : S)       = NIL
;

fun QUOTE (exp : S) (env : S) = exp;
;

local
    exception FAIL of string;
in
    fun PLUS (INT a, INT b) (env : S) = INT (a + b)
    |   PLUS (_, _) env               =
            raise FAIL "PLUS:: both arguments should be of type INT"
    ;

    fun TIMES (INT a, INT b) (env : S) = INT (a * b)
    |   TIMES (_, _) (env : S)         =
            raise FAIL "TIMES:: both arguments should be of type INT"
    ;

    fun MEANING (_, NIL) (env : S)                   = NIL
    |   MEANING (STR IDENTIFIER, BINDINGS) (env : S) =
            let
                val STR binding = CAR (CAR BINDINGS NIL) NIL
            in
                if binding = IDENTIFIER then
                    CDR (CAR BINDINGS NIL) NIL
                else
                    MEANING (STR IDENTIFIER, CDR BINDINGS NIL) env
            end
    |   MEANING (_, _) (env : S) =
            raise FAIL "MEANING:: arguments should be of type STR * S"
    ;
end;

local
    exception FAIL of string

    (*
        check if a parameter is considered atomic in mlisp.
    *)
    fun is_atomic NIL     = T
    |   is_atomic (INT _) = T
    |   is_atomic (STR _) = T
    |   is_atomic (EAGER _) = T
    |   is_atomic (NORMAL _) = T
    |   is_atomic _       = NIL
    ;

    (*
        since S values are not comparable, this internal function is the
        alternative for internal compartions
    *)
    fun EQ_internal (INT a, INT b) =
            a = b
    |   EQ_internal (STR a, STR b) =
            a = b
    |   EQ_internal (NIL, NIL) =
            true
    |   EQ_internal (CONS(car1, cdr1), CONS(car2, cdr2)) =
            EQ_internal (car1, car2) andalso EQ_internal (cdr1, cdr2)
    |   EQ_internal (_, _) =
            false
    ;

    (*
        used in EVAL to make sure the number of arguments matches
        the function's type.
    *)
    fun count_args NIL = 0
    |   count_args args =
            1 + count_args (CDR args NIL)
    ;

in
    (* note that it does not support comparing two functions *)
    fun EQ (a, b) (env : S) =
        if EQ_internal(a, b) then
            T
        else
            NIL
    ;

    fun LST NIL (env : S)              = T
    |   LST (CONS (NIL, cdr)) (env : S)  = LST cdr env
    |   LST (CONS (car, NIL)) (env : S)  =
        if EQ_internal(is_atomic car, T) orelse EQ_internal(LST car env, T) then
            T
        else
            NIL
    |   LST (CONS (car, cdr)) (env : S)  =
        if EQ_internal(LST car env, T) orelse EQ_internal(is_atomic car, T) andalso EQ_internal(LST cdr env, T) then
            T
        else
            NIL
    |   LST _ (env : S)                  = NIL
    ;

    fun EVAL (NIL, _)                               =
            NIL
    |   EVAL (INT s, _)                             =
            INT s
    |   EVAL (STR s, env)                           =
            MEANING (STR s, env) env

    (* EAGER functions *)
    |   EVAL (CONS(EAGER (UNARY f), arg), env:S)    =
            if count_args arg <> 1 then
                raise FAIL "EVAL:: function is UNARY but the number of arguments is not 1"
            else
                f (EVAL(CAR arg env, env)) env

    |   EVAL (CONS (EAGER (BINARY f), args), env)   =
            if count_args args <> 2 then
                raise FAIL "EVAL:: function is BINARY but the number of arguments is not 2"
            else
                f (EVAL (CAR args env, env),
                   EVAL(CAR (CDR args env) env, env))
                env

    |   EVAL (CONS (EAGER (TRINARY f), args), env)  =
            if count_args args <> 3 then
                raise FAIL "EVAL:: function is TRINARY but the number of arguments is not 3"
            else
                f (EVAL (CAR args env, env),
                   EVAL(CAR (CDR args env) env, env),
                   EVAL(CAR (CDR (CDR args NIL) NIL) NIL, env))
                env

    (* NORMAL functions *)
    |   EVAL (CONS(NORMAL (UNARY f), arg), env)     =
            if count_args arg <> 1 then
                raise FAIL "EVAL:: function is UNARY but the number of arguments is not 1"
            else
                f (CAR arg env) env

    |   EVAL (CONS (NORMAL (BINARY f), args), env)  =
            if count_args args <> 2 then
                raise FAIL "EVAL:: function is BINARY but the number of arguments is not 2"
            else
                f (CAR args env,
                   CAR (CDR args env) env) env

    |   EVAL (CONS (NORMAL (TRINARY f), args), env) =
            if count_args args <> 3 then
                raise FAIL "EVAL:: function is TRINARY but the number of arguments is not 3"
            else
                f (CAR args env,
                   CAR (CDR args env) env,
                   CAR (CDR (CDR args env) env) env)
                env

    (* if all pattern matching failed, then the first eval param is not valid *)
    |   EVAL (_, _)                                 =
            raise FAIL "EVAL:: no type match found"

    (*
        uses EVAL in order to allow it to be NORMAL evaluation. this is
        also the reasonable way to do it, i.e. do a lazy evaluation only on the
        argument you actually need to evaluate and not the other.
    *)
    fun COND (first, second : S, third : S) (env : S) =
        if EQ_internal(EVAL(first, env), NIL) then
            EVAL(third, env)
        else
            EVAL(second, env)
    ;
end;

(*
    the idea is to create another node at the top,
    connecing CAR to K.V and CDR to env
*)
fun SETQ (K, V) env =
    let
        val V = EVAL(V, env)
    in
        CONS (CONS (K, V), env)
    end
;
