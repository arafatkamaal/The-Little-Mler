
datatype fruit =
    Peach
  | Apple
  | Pear
  | Lemon
  | Fig;

datatype tree =
    Bud
  | Flat  of fruit * tree
  | Split of tree  * tree;

(*
** Checking the shape of flatness and split
*)
val flat_pear =
Flat(Pear,
    Bud);

val split_fig = 
Split(
    Bud,
    Flat(Fig,
        Split(
            Bud,
            Bud)));

val split_lemon =
Split(
    Split(
        Bud,
        Flat(Lemon, Bud)),
    Flat(Fig,
        Split(
            Bud,
            Bud)));

(*
** Like only_onions
** fn : tree -> bool
*)
fun flat_only(Bud)         = true
  | flat_only(Flat(f, t))  = flat_only(t)
  | flat_only(Split(s, t)) = false;

flat_only(flat_pear);
flat_only(split_fig);
flat_only(split_lemon);

fun split_only(Bud)        = true
  | split_only(Flat(f, t)) = false
  | split_only(Split(s, t))
    = if split_only(s)
      then split_only(t)
      else false;

split_only(Bud);

val full_of_splits =
Split(
    Split(
        Bud,
        Split(
            Bud,
            Bud)),
    Split(
        Bud,
        Split(
            Bud,
            Bud)));

split_only(full_of_splits);

val splits_with_flats =
Split(
    Split(
        Bud,
        Flat(
            Peach,
            Bud)),
    Split(
        Bud,
        Split(
            Bud,
            Bud)));

split_only(splits_with_flats);

(*
** From very defintion a split_only tree can never have a fruit
*)
fun contains_fruit(f)
    = if split_only(f)
        then false
        else true;

(*
** Entering the world of heights
*)
fun larger_of(n: int, m: int)
    = if n < m
        then m
        else n;

larger_of(100, 9);

(*
** Follows from:
**  a. Bud is at zero so = 0
**  b. Flat(adds a 1 to the height) contains a fruit(deadend), and tree(its height), 1 + height of tree
**  c. Split(adds a 1 to the height) contains two trees, one of them could be longer, so 1 + larger of first and second tree 
*)
fun height(Bud)         = 0
  | height(Flat(f, t))  = 1 + height(t)
  | height(Split(s, t)) = 1 + larger_of(height(s), height(t));

height(Bud);
height(split_fig);
height(flat_pear);

val large_fig_tree =
Split(
    Split(
        Flat(Fig,
            Bud),
        Flat(Fig,
            Bud)),
    Flat(Fig,
        Flat(Lemon,
            Flat(Apple,
                Bud))));

(*
** Follows from:
**  a. Bud, gives Bud, obviously as there is nothing to replace
**  b. Flat, contain a fruit, for sure, so check if it matches with our candidate fruit and return a Flat with replace/original fruit
**  c. Split, contains two trees, they could have flats, or buds, Just call the same function for both the trees
*) 

fun eql_fruit(Peach, Peach) = true
  | eql_fruit(Apple, Apple) = true
  | eql_fruit(Pear,  Pear)  = true
  | eql_fruit(Lemon, Lemon) = true
  | eql_fruit(Fig,   Fig)   = true
  | eql_fruit(f,     sof)   = false;

fun subst_in_tree(wf, f, Bud)         = Bud
  | subst_in_tree(wf, f, Flat(sof, t))
    = if eql_fruit(f, sof)
        then Flat(wf, subst_in_tree(wf, f, t))
        else Flat(f,  subst_in_tree(wf, f, t))
  | subst_in_tree(wf, f, Split(s, t)) = Split(subst_in_tree(wf, f, s), subst_in_tree(wf, f, t));

subst_in_tree(Apple, Fig, large_fig_tree);


(*
** Follow from
**  a. Bud, dead tree, contributes 0
**  b. Flat has a fruit and a tree, 
***      if it matches our candidate fruit add 1 with further occurances of fruit in the tree, 
***      else contribute zero and continue hunting remaining occurances of the fruit in the tree
**  c. Split contains two trees and no fruits, contribute 0 and they could contain flats in the remaining two trees, so work on them
*)
fun occurs(f, Bud) = 0
  | occurs(f, Flat(sof, t))
    = if eql_fruit(f, sof)
        then 1 + occurs(f, t)
        else 0 + occurs(f, t)
  | occurs(f, Split(s, t)) = 0 + occurs(f, s) + occurs(f, t);

occurs(Fig, large_fig_tree);


(*
** Time to up the game
*)

(*
** Mutually self referenced types
***** Rule: Mutually self reference types lead to mutually self referenced funtions
*)
datatype
    'a slist =
        Empty
      | Scons of (('a sexp) * ('a slist))
  and
    'a sexp =
        An_atom of 'a
      | A_slist of ('a slist);

An_atom(5); (*int sexp*)
An_atom(Fig); (*fruit sexp*)
A_slist(Empty);

Scons(An_atom(5),
    Scons(An_atom(13),
        Scons(An_atom(1), Empty)));

Scons(An_atom(Fig), Empty);

val fig_cons_tree =
Scons(An_atom(Fig),
    Scons(An_atom(Fig),
        Scons(An_atom(Lemon), Empty)));

val mixed_fig_cons_tree =
Scons(A_slist(
        Scons(An_atom(Fig),
            Scons(An_atom(Peach),
                Empty))),
    Scons(An_atom(Fig),
        Scons(An_atom(Lemon), Empty)));



fun occurs_in_slist(a, Empty)       = 0 
  | occurs_in_slist(a, Scons(s, y)) = occurs_in_sexp(a, s) + occurs_in_slist(a, y)
and 
    occurs_in_sexp(a, An_atom(b))
    = if eql_fruit(a, b)
        then 1
        else 0
  | occurs_in_sexp(a, A_slist(y)) = occurs_in_slist(a, y);

occurs_in_slist(Fig, fig_cons_tree);

fun subst_in_slist(n, a, Empty)       = Empty
 |  subst_in_slist(n, a, Scons(s, y)) = Scons(subst_in_sexp(n, a, s),  subst_in_slist(n, a, y))
and
    subst_in_sexp(n, a, An_atom(b))
    = if eql_fruit(a, b)
        then An_atom(n)
        else An_atom(b)
 |  subst_in_sexp(n, a, A_slist(y)) = A_slist(subst_in_slist(n, a, y));

subst_in_slist(Peach, Fig, fig_cons_tree);

(*
** Removal is a little tricky
*)

fun eq_fruit_in_atom(a, An_atom(s)) = eql_fruit(a, s)
  | eq_fruit_in_atom(a, A_slist(y)) = false;

fun remove_from_slist(a, Empty)       = Empty
 |  remove_from_slist(a, Scons(s, y))
    = if eq_fruit_in_atom(a, s)
        then remove_from_slist(a, y)
        else Scons(remove_from_sexp(a, s), remove_from_slist(a, y))
and
    remove_from_sexp(a, An_atom(b)) = An_atom(b)
 |  remove_from_sexp(a, A_slist(y)) = A_slist(remove_from_slist(a, y));

remove_from_slist(Fig, fig_cons_tree);
remove_from_slist(Fig, mixed_fig_cons_tree);

(*Removing mutual recusion*)

fun remove_from_slist(a, Empty) = Empty
 |  remove_from_slist(a, Scons(An_atom(b), y))
    = if eql_fruit(a, b)
        then remove_from_slist(a, y)
        else Scons(An_atom(b), remove_from_slist(a, y))
 |  remove_from_slist(a, Scons(A_slist(x), y))
    = Scons(A_slist(remove_from_slist(a, x)), remove_from_slist(a, y));

remove_from_slist(Fig, fig_cons_tree);
remove_from_slist(Fig, mixed_fig_cons_tree);

(* Output of code from this file
$ sml < 06-Oh-my-its-full-of-stars.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- datatype fruit = Apple | Fig | Lemon | Peach | Pear
datatype tree = Bud | Flat of fruit * tree | Split of tree * tree
val flat_pear = Flat (Pear,Bud) : tree
val split_fig = Split (Bud,Flat (Fig,Split #)) : tree
val split_lemon = Split (Split (Bud,Flat #),Flat (Fig,Split #)) : tree
val flat_only = fn : tree -> bool
val it = true : bool
val it = false : bool
val it = false : bool
val split_only = fn : tree -> bool
val it = true : bool
val full_of_splits = Split (Split (Bud,Split #),Split (Bud,Split #)) : tree
val it = true : bool
val splits_with_flats = Split (Split (Bud,Flat #),Split (Bud,Split #)) : tree
val it = false : bool
val contains_fruit = fn : tree -> bool
val larger_of = fn : int * int -> int
val it = 100 : int
val height = fn : tree -> int
val it = 0 : int
val it = 3 : int
val it = 1 : int
val large_fig_tree = Split (Split (Flat #,Flat #),Flat (Fig,Flat #)) : tree
val eql_fruit = fn : fruit * fruit -> bool
val subst_in_tree = fn : fruit * fruit * tree -> tree
val it = Split (Split (Flat #,Flat #),Flat (Apple,Flat #)) : tree
val occurs = fn : fruit * tree -> int
val it = 3 : int
datatype 'a slist = Empty | Scons of 'a sexp * 'a slist
datatype 'a sexp = A_slist of 'a slist | An_atom of 'a
val it = An_atom 5 : int sexp
val it = An_atom Fig : fruit sexp
val it = A_slist Empty : 'a sexp
val it = Scons (An_atom 5,Scons (An_atom #,Scons #)) : int slist
val it = Scons (An_atom Fig,Empty) : fruit slist
val fig_cons_tree = Scons (An_atom Fig,Scons (An_atom #,Scons #))
  : fruit slist
val mixed_fig_cons_tree =
  Scons (A_slist (Scons (#,#)),Scons (An_atom #,Scons #)) : fruit slist
val occurs_in_slist = fn : fruit * fruit slist -> int
val occurs_in_sexp = fn : fruit * fruit sexp -> int
val it = 2 : int
val subst_in_slist = fn : fruit * fruit * fruit slist -> fruit slist
val subst_in_sexp = fn : fruit * fruit * fruit sexp -> fruit sexp
val it = Scons (An_atom Peach,Scons (An_atom #,Scons #)) : fruit slist
val eq_fruit_in_atom = fn : fruit * fruit sexp -> bool
val remove_from_slist = fn : fruit * fruit slist -> fruit slist
val remove_from_sexp = fn : fruit * fruit sexp -> fruit sexp
val it = Scons (An_atom Lemon,Empty) : fruit slist
val it = Scons (A_slist (Scons (#,#)),Scons (An_atom #,Empty)) : fruit slist
val remove_from_slist = fn : fruit * fruit slist -> fruit slist
val it = Scons (An_atom Lemon,Empty) : fruit slist
val it = Scons (A_slist (Scons (#,#)),Scons (An_atom #,Empty)) : fruit slist
*)