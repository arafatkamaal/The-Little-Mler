
Control.Print.printDepth := 20;

(*Normal list*)
datatype 'a list =
    Empty
  | Cons of 'a * 'a list;

(*Type with index*)
datatype box =
    Bacon
  | Ix of int;

val three_bacon =
Cons(Ix(5),
    Cons(Ix(13),
        Cons(Bacon,
            Cons(Ix(8), Empty))));

val one_bacon =
Cons(Bacon,
    Cons(Ix(8),
        Empty));

val zero_bacon =
Cons(Ix(5),
    Cons(Ix(13),
        Cons(Ix(8),
            Empty)));

fun is_bacon(Bacon) = true
  | is_bacon(Ix(n)) = false;

fun where_is(Empty) = 0
  | where_is(Cons(a_box, rest))
    = if is_bacon(a_box)
        then 1
        else 1 + where_is(rest);

where_is(three_bacon);
where_is(one_bacon);
where_is(zero_bacon);

(*We have to work with exceptions since the last value is wrong*)
exception No_bacon of int;
fun where_is(Empty) = raise No_bacon(0)
  | where_is(Cons(a_box, rest))
    = if is_bacon(a_box)
        then 1
        else 1 + where_is(rest);

where_is(three_bacon);
where_is(one_bacon);
(* It is going to fail here
**  where_is(zero_bacon);
*)
(where_is(zero_bacon)
 handle
    No_bacon(an_int)
    => an_int);

(*
** function to find bacon
*)
exception Out_of_range;
fun list_item(n, Empty) = raise Out_of_range
  | list_item(n, Cons(abox, rest))
    = if n = 1
        then abox
        else list_item(n - 1, rest);

(*first cut*)
fun find(n, boxes)         = check(n, boxes, list_item(n, boxes))
 and
    check(n, boxes, Bacon) = n
  | check(n, boxes, Ix(i)) = find(i, boxes);

(*With exception handling*)
fun find(n, boxes)
    = (check(n, boxes, list_item(n, boxes))
        handle
          Out_of_range
            => find(n div 2, boxes))
 and
    check(n, boxes, Bacon) = n
  | check(n, boxes, Ix(i)) = find(i, boxes);

(*
** Code to trace the path the of check
*) 
fun path(n, boxes)
    = Cons(n,
        (check(boxes, list_item(n, boxes))
          handle
            Out_of_range
              => path(n div 2, boxes)))
 and
    check(boxes, Bacon) = Empty
  | check(boxes, Ix(i)) = path(i, boxes);

(*Output of code from this file
$ sml < 09-oh-no.sml 
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
datatype box = Bacon | Ix of int
val three_bacon = Cons (Ix 5,Cons (Ix 13,Cons (Bacon,Cons (Ix 8,Empty))))
  : box list
val one_bacon = Cons (Bacon,Cons (Ix 8,Empty)) : box list
val zero_bacon = Cons (Ix 5,Cons (Ix 13,Cons (Ix 8,Empty))) : box list
val is_bacon = fn : box -> bool
val where_is = fn : box list -> int
val it = 3 : int
val it = 1 : int
val it = 3 : int
exception No_bacon of int
val where_is = fn : box list -> int
val it = 3 : int
val it = 1 : int
val it = 0 : int
exception Out_of_range
val list_item = fn : int * 'a list -> 'a
val find = fn : int * box list -> int
val check = fn : int * box list * box -> int
val find = fn : int * box list -> int
val check = fn : int * box list * box -> int
val path = fn : int * box list -> int list
val check = fn : box list * box -> int list
*)