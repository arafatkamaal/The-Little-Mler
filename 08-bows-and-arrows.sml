
Control.Print.printDepth := 100;

datatype 'a list =
    Empty
  | Cons of 'a * 'a list;

Cons(1,
    Cons(2,
        Cons(3,
            Cons(4, Empty))));

(*
** Apples to oranges comparison
*)
datatype orapl =
    Orange
  | Apple;

fun eq_orapl(Orange, Orange) = true
  | eq_orapl(Apple,  Apple)  = true
  | eq_orapl(onef,   otherf) = false;

(*
** substitue functions
*)
fun eq_int(n, m)    = n = m;
fun less_than(n, m) = n < m;

fun subst_int(n, a, Empty)      = Empty
  | subst_int(n, a, Cons(e, t))
    = if a = e
        then Cons(n, subst_int(n, a, t))
        else Cons(e, subst_int(n, a, t));

subst_int(2, 3,
Cons(1,
    Cons(2,
        Cons(3,
            Cons(4, Empty)))));

fun sub_orapl(n, a, Empty) = Empty
  | sub_orapl(n, a, Cons(e, t))
    = if eq_orapl(a, e)
        then Cons(n, sub_orapl(n, a, t))
        else Cons(e, sub_orapl(n, a, t));

subst_int(Apple, Orange,
Cons(Apple,
    Cons(Orange,
        Cons(Apple,
            Cons(Orange, Empty)))));
    
(*
** Do not repeat yourself, duplicate code is stupid code
**   fn : ('a * 'b -> bool) * 'b * 'a * 'b list -> 'b list
**        fn with two args    any  any  any list
**        and returns bool
*) 
fun subst(rel, n, a, Empty) = Empty
  | subst(rel, n, a, Cons(e, t))
    = if rel(a, e)
        then Cons(n, subst(rel, n, a, t))
        else Cons(e, subst(rel, n, a, t));

subst(eq_orapl, Apple, Orange,
Cons(Apple,
    Cons(Orange,
        Cons(Apple,
            Cons(Orange, Empty)))));

subst(eq_int, 2, 3,
Cons(1,
    Cons(2,
        Cons(3,
            Cons(4, Empty)))));

subst(less_than, 11, 15,
Cons(15,
    Cons(6,
        Cons(15,
            Cons(17,
                Cons(15,
                    Cons(8, Empty)))))));    

fun int_range((s, e), c)
    = if less_than(s, c)
        then less_than(c, e)
        else false;

int_range((1, 3), 2);
int_range((1, 31), 200);

(*
** Substitute in range is going to be trickier to right
*)
fun subst_pred(pred, n, Empty) = Empty
  | subst_pred(pred, n, Cons(e, t))
    = if pred(e)
        then Cons(n, subst_pred(pred, n, t))
        else Cons(e, subst_pred(pred, n, t));

fun is_15(n)          = eq_int(n, 15);
fun less_than_15(n)   = less_than(n, 15);
fun in_range_11_16(n) = int_range((11, 16), n);

in_range_11_16(8);
in_range_11_16(17);

subst_pred(is_15, 11,
Cons(15,
    Cons(6,
        Cons(15,
            Cons(17,
                Cons(15,
                    Cons(8, Empty)))))));  

subst_pred(less_than_15, 11,
Cons(15,
    Cons(6,
        Cons(15,
            Cons(17,
                Cons(15,
                    Cons(8, Empty)))))));

subst_pred(in_range_11_16, 15,
Cons(15,
    Cons(11,
        Cons(15,
            Cons(17,
                Cons(15,
                    Cons(14, Empty)))))));

(*
** In range function
*)
fun in_range_c(s, e)(c)
    = if less_than(s, c)
        then less_than(c, e)
        else false;

in_range_c(11, 16)(13);
in_range_c(100, 1000)(500);
in_range_c(2012, 3000)(2);

subst_pred(in_range_c(11, 16), 15,
Cons(15,
    Cons(11,
        Cons(15,
            Cons(17,
                Cons(15,
                    Cons(14, Empty)))))));

(*
** A new subst
*)
fun subst_c(pred)(n, Empty) = Empty
  | subst_c(pred)(n, Cons(e, t))
    = if pred(e)
        then Cons(n, subst_c(pred)(n, t))
        else Cons(e, subst_c(pred)(n, t));

(*
** Combine functions
*)
fun combine(Empty, Empty)             = Empty
  | combine(Empty, Cons(b, l2))       = Cons(b, l2)
  | combine(Cons(a, l1), Empty)       = Cons(a, l1)
  | combine(Cons(a, l1), Cons(b, l2)) = Cons(a, combine(l1, Cons(b, l2)));

fun combine(Empty, l2)       = l2
  | combine(Cons(a, l1), l2) = Cons(a, combine(l1, l2));

combine(
    Cons(1,
        Cons(2,
            Cons(3,
                Empty))),
    Cons(5,
        Cons(4,
            Cons(7,
                Cons(9,
                    Empty)))));

fun combine_c(Empty)(l2)       = l2
  | combine_c(Cons(a, l1))(l2) = Cons(a, combine_c(l1)(l2));

combine_c(
    Cons(1,
        Cons(2,
            Cons(3, Empty))));

fun prefixer_123(l2)
    = Cons(1,
        Cons(2,
          Cons(3, l2)));

prefixer_123(Empty);

fun waiting_prefix_123(l2)
    = Cons(1,
        combine_c(
            Cons(2,
                Cons(3, Empty)))(l2));

waiting_prefix_123(Empty);

fun base(l2) = l2;
fun combine_s(Empty)       = base
  | combine_s(Cons(a, l1)) = make_cons(a, combine_s(l1))
and
    make_cons(a, f)(l2)    = Cons(a, f(l2)); 

combine_s(
    Cons(1,
        Cons(2,
            Cons(3, Empty))));

fun prefix_3(l2)   = Cons(3, base(l2));
fun prefix_23(l2)  = Cons(2, prefix_3(l2));
fun prefix_123(l2) = Cons(1, prefix_23(l2));


prefix_3(Cons(1, Empty));
prefix_123(Cons(4, Empty));

(* Output of code from this file
$ sml < 08-bows-and-arrows.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- [autoloading]
[library $smlnj/compiler/current.cm is stable]
[library $smlnj/compiler/x86.cm is stable]
[library $smlnj/viscomp/core.cm is stable]
[library $smlnj/viscomp/basics.cm is stable]
[library $smlnj/viscomp/elabdata.cm is stable]
[library $smlnj/viscomp/elaborate.cm is stable]
[library $compiler/(core.cm):MiscUtil/const-arith/sources.cm is stable]
[library $smlnj/MLRISC/Lib.cm is stable]
[library $SMLNJ-MLRISC/Lib.cm is stable]
[library $smlnj/viscomp/debugprof.cm is stable]
[library $SMLNJ-LIB/Util/smlnj-lib.cm is stable]
[library $SMLNJ-BASIS/basis.cm is stable]
[library $SMLNJ-BASIS/(basis.cm):basis-common.cm is stable]
[library $smlnj/smlnj-lib/pp-lib.cm is stable]
[library $SMLNJ-LIB/PP/pp-lib.cm is stable]
[library $html-lib.cm(=$SMLNJ-LIB/HTML)/html-lib.cm is stable]
[library $smlnj/MLRISC/Control.cm is stable]
[library $SMLNJ-MLRISC/Control.cm is stable]
[library $controls-lib.cm(=$SMLNJ-LIB/Controls)/controls-lib.cm is stable]
[library $smlnj/smlnj-lib/controls-lib.cm is stable]
[autoloading done]
val it = () : unit
datatype 'a list = Cons of 'a * 'a list | Empty
val it = Cons (1,Cons (2,Cons (3,Cons (4,Empty)))) : int list
datatype orapl = Apple | Orange
val eq_orapl = fn : orapl * orapl -> bool
stdIn:27.25 Warning: calling polyEqual
val eq_int = fn : ''a * ''a -> bool
val less_than = fn : int * int -> bool
stdIn:32.12 Warning: calling polyEqual
val subst_int = fn : ''a * ''a * ''a list -> ''a list
val it = Cons (1,Cons (2,Cons (2,Cons (4,Empty)))) : int list
val sub_orapl = fn : orapl * orapl * orapl list -> orapl list
val it = Cons (Apple,Cons (Apple,Cons (Apple,Cons (Apple,Empty))))
  : orapl list
val subst = fn : ('a * 'b -> bool) * 'b * 'a * 'b list -> 'b list
val it = Cons (Apple,Cons (Apple,Cons (Apple,Cons (Apple,Empty))))
  : orapl list
val it = Cons (1,Cons (2,Cons (2,Cons (4,Empty)))) : int list
val it = Cons (15,Cons (6,Cons (15,Cons (11,Cons (15,Cons (8,Empty))))))
  : int list
val int_range = fn : (int * int) * int -> bool
val it = true : bool
val it = false : bool
val subst_pred = fn : ('a -> bool) * 'a * 'a list -> 'a list
val is_15 = fn : int -> bool
val less_than_15 = fn : int -> bool
val in_range_11_16 = fn : int -> bool
val it = false : bool
val it = false : bool
val it = Cons (11,Cons (6,Cons (11,Cons (17,Cons (11,Cons (8,Empty))))))
  : int list
val it = Cons (15,Cons (11,Cons (15,Cons (17,Cons (15,Cons (11,Empty))))))
  : int list
val it = Cons (15,Cons (11,Cons (15,Cons (17,Cons (15,Cons (15,Empty))))))
  : int list
val in_range_c = fn : int * int -> int -> bool
val it = true : bool
val it = true : bool
val it = false : bool
val it = Cons (15,Cons (11,Cons (15,Cons (17,Cons (15,Cons (15,Empty))))))
  : int list
val subst_c = fn : ('a -> bool) -> 'a * 'a list -> 'a list
val combine = fn : 'a list * 'a list -> 'a list
val combine = fn : 'a list * 'a list -> 'a list
val it = Cons (1,Cons (2,Cons (3,Cons (5,Cons (4,Cons (7,Cons (9,Empty)))))))
  : int list
val combine_c = fn : 'a list -> 'a list -> 'a list
val it = fn : int list -> int list
val prefixer_123 = fn : int list -> int list
val it = Cons (1,Cons (2,Cons (3,Empty))) : int list
val waiting_prefix_123 = fn : int list -> int list
val it = Cons (1,Cons (2,Cons (3,Empty))) : int list
val base = fn : 'a -> 'a
val combine_s = fn : 'a list -> 'a list -> 'a list
val make_cons = fn : 'a * ('a list -> 'a list) -> 'a list -> 'a list
val it = fn : int list -> int list
val prefix_3 = fn : int list -> int list
val prefix_23 = fn : int list -> int list
val prefix_123 = fn : int list -> int list
val it = Cons (3,Cons (1,Empty)) : int list
val it = Cons (1,Cons (2,Cons (3,Cons (4,Empty)))) : int list
*)