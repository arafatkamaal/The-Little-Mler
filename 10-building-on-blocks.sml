
Control.Print.printDepth := 20;

fun is_zero(n)   = n = 0;
fun eq_int(m, n) = m = n;
fun succ(n)      = n + 1;

exception Too_small;
fun pred(n)
    = if eq_int(n, 0)
        then raise Too_small
        else n - 1;

fun plus(n, m)
    = if is_zero(n)
        then m
        else succ(plus(pred(n), m));

plus(0, 1);
plus(1, 1);
plus(2, 1);
plus(3, 2);

(*
**Defining plus in terms of earlier datatype num
*)
datatype num =
    Zero
  | One_more_than of num;

fun is_zero(Zero)     = true
  | is_zero(not_zero) = false;

fun pred(Zero)             = raise Too_small
  | pred(One_more_than(n)) = n;

fun succ(n) = One_more_than(n);

fun plus(n, m)
    = if is_zero(n)
        then m
        else succ(plus(pred(n), m));

val our_first_number =
One_more_than(
    One_more_than(
        Zero));

val our_second_number =
One_more_than(
    One_more_than(
        One_more_than(
            Zero)));

plus(our_first_number, our_second_number);

signature N =
sig
  type number
  exception Too_small
  val succ :   number -> number
  val pred :   number -> number
  val is_zero: number -> bool
end;

functor NumberAsNum()
    :>
    N
    =
    struct
        datatype num =
            Zero
          | One_more_than of num
        type number = num
        exception Too_small
        fun succ(n) = One_more_than(n)
        fun pred(Zero)             = raise Too_small
          | pred(One_more_than(n)) = n
        fun is_zero(Zero)                 = true
          | is_zero(One_more_than(a_num)) = false    
    end;

functor NumberAsInt()
    :>
    N
    =
    struct
        type number = int
        exception Too_small
        fun succ(n) = n + 1
        fun pred(n)
            = if eq_int(0, n)
                then raise Too_small
                else n -1
        fun is_zero(n) = eq_int(0, n)
    end;


(*
** Attempting to use plus for all types
*)
structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();

signature P =
    sig
        type number
        val plus : (number * number) -> number
    end;

functor PON(structure a_N: N)
    :>
    P
    =
    struct
        type number = a_N.number
        fun plus(n, m)
            = if a_N.is_zero(n)
                then m
                else a_N.succ(plus(a_N.pred(n), m))
    end;

structure IntArith = PON(structure a_N = IntStruct);
structure NumArith = PON(structure a_N = NumStruct);

(* Fails!
IntArith.plus(1, 2);
*)

signature N_C_R =
    sig
        type number
        exception Too_small
        val conceal: int -> number
        val succ:    number -> number
        val pred:    number -> number
        val is_zero: number -> bool
        val reveal:  number -> int
    end;

functor NumberAsInt()
    :>
    N_C_R
    =
    struct
        type number = int
        exception Too_small
        fun conceal(n) = n
        fun succ(n) = n + 1
        fun pred(n)
            = if eq_int(n, 0)
                then raise Too_small
                else n - 1
        fun is_zero(n) = eq_int(n, 0)
        fun reveal(n) = n
    end;

functor NumberAsNum()
    :>
    N_C_R
    =
    struct
        datatype num =
            Zero
          | One_more_than of num
        type number = num
        exception Too_small
        fun conceal(n)
            = if eq_int(n, 0)
                then Zero
                else One_more_than(conceal(n - 1))
        fun succ(n) = One_more_than(n)
        fun pred(Zero)             = raise Too_small
          | pred(One_more_than(n)) = n
        fun is_zero(Zero)  = true
          | is_zero(a_num) = false
        fun reveal(n)
            = if is_zero(n)
                then 0
                else 1 + reveal(pred(n))
    end;

structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();
structure IntArith = PON(structure a_N = IntStruct);
structure NumArith = PON(structure a_N = NumStruct);

NumStruct.reveal(
    NumStruct.succ(
        NumStruct.conceal(0)));

(* Doesn't work
NumStruct.reveal(
    NumStruct.plus(
        NumStruct.conceal(1),
        NumStruct.conceal(2)));
*)
functor PON(structure a_N: N)
    :>
    P where type number = a_N.number
    =
    struct
        type number = a_N.number
        fun plus(n, m)
            = if a_N.is_zero(n)
                then m
                else a_N.succ(plus(a_N.pred(n), m));
    end;

structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();
structure IntArith = PON(structure a_N = IntStruct);
structure NumArith = PON(structure a_N = NumStruct);

NumStruct.reveal(
    NumStruct.succ(
        NumStruct.conceal(0)));

