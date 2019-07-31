
(*
** This basically consists
***** 1. 'a pizza
***** 2.  A function that takes in (anything, pizza) and returns a pizza
*)

datatype 'a pizza =
    Bottom                        (*'a pizza*)
  | Topping of ('a * ('a pizza)); (*(fn : 'a * 'a pizza -> 'a pizza)*)

datatype fish =
    Anchovy
  | Lox
  | Tuna;

Bottom;
Topping;

(*
** Topping (Anchovy,Bottom) : fish pizza
*)
Topping(Anchovy, Bottom);

(*more*)
Topping(Anchovy,
    Topping(Tuna,
        Topping(Anchovy,
            Bottom)));

fun remove_anchovy(Bottom)              = Bottom
  | remove_anchovy(Topping(Anchovy, p)) = remove_anchovy(p)
  | remove_anchovy(Topping(Tuna,p))     = Topping(Tuna, remove_anchovy(p))
  | remove_anchovy(Topping(Lox, p))     = Topping(Lox,  remove_anchovy(p));

remove_anchovy(
    Topping(Lox,
        Topping(Anchovy,
            Topping(Tuna,
                Topping(Anchovy,
                    Bottom))))); (* Topping (Lox,Topping (Tuna,Bottom)) : fish pizza *)

remove_anchovy(
    Topping(Lox,
        Topping(Tuna,
            Bottom))); (*Works*)

(*
** Shorter version
*)
fun remove_anchovy(Bottom)              = Bottom
  | remove_anchovy(Topping(Anchovy, p)) = remove_anchovy(p)
  | remove_anchovy(Topping(f, t))       = Topping(f, remove_anchovy(t));

remove_anchovy(
    Topping(Lox,
        Topping(Anchovy,
            Topping(Tuna,
                Topping(Anchovy,
                    Bottom))))); (* Topping (Lox,Topping (Tuna,Bottom)) : fish pizza *)

remove_anchovy(
    Topping(Lox,
        Topping(Tuna,
            Bottom))); (*Works*)

(*
** Remove any fish
*)
fun rem_fish(x,         Bottom)                      = Bottom
  | rem_fish(Tuna,      Topping(Tuna, p))            = rem_fish(Tuna, p)
  | rem_fish(Anchovy,   Topping(Anchovy, p))         = rem_fish(Anchovy, p)
  | rem_fish(Lox,       Topping(Lox, p))             = rem_fish(Lox, p)
  | rem_fish(some_fish, Topping(some_other_fish, p)) = Topping(some_other_fish, rem_fish(some_fish, p));

rem_fish(Lox,
    Topping(Lox,
        Topping(Tuna,
            Bottom)));

rem_fish(Anchovy,
    Topping(Lox,
        Topping(Anchovy,
            Topping(Tuna,
                Topping(Anchovy,
                    Bottom)))));

(* Ungrammatical apparently
fun rem_fish(f, Bottom)          = Bottom
  | rem_fish(f, Topping(f,   p)) = rem_fish(f, p)
  | rem_fish(f, Topping(sof, p)) = Topping(sof, rem_fish(f, p));
*)

(*
** Correc short definition
*)
fun eq_fish(Anchovy, Anchovy) = true
  | eq_fish(Lox,     Lox)     = true
  | eq_fish(Tuna,    Tuna)    = true
  | eq_fish(f, sof)           = false;

eq_fish;
eq_fish(Anchovy, Anchovy);
eq_fish(Anchovy, Lox);

fun rem_fish(f, Bottom) = Bottom
  | rem_fish(f, Topping(sof, p))
    = if eq_fish(f, sof)
       then rem_fish(f, p)
       else Topping(sof, rem_fish(f, p));

rem_fish(Lox,
    Topping(Lox,
        Topping(Tuna,
            Bottom)));

rem_fish(Anchovy,
    Topping(Lox,
        Topping(Anchovy,
            Topping(Tuna,
                Topping(Anchovy,
                    Bottom)))));

(*
** With other collection types
*)
fun eq_int(n: int, m: int) = (n = m);
fun rem_int(i, Bottom) = Bottom
  | rem_int(i, Topping(soi, p))
    = if eq_int(i, soi)
       then rem_int(i, p)
       else Topping(soi, rem_int(i, p));

rem_int(1,
    Topping(1,
        Topping(2,
            Bottom)));

rem_int(1,
    Topping(2,
        Topping(1,
            Topping(3,
                Topping(1,
                    Bottom)))));

(*
** Substitue things in toppings
*)
fun subst_fish(wf, f, Bottom) = Bottom
  | subst_fish(wf, f, Topping(sof, p))
    = if eq_fish(f, sof)
       then Topping(wf,  subst_fish(wf, f, p))
       else Topping(sof, subst_fish(wf, f, p));

subst_fish(Lox, Anchovy,
    Topping(Lox,
        Topping(Anchovy,
            Topping(Tuna,
                Topping(Anchovy,
                    Bottom)))));

(*
** Some more of this
*)
datatype num =
    Zero
  | One_more_than of num;

fun eq_num(Zero, Zero)                         = true
  | eq_num(Zero, One_more_than(x))             = false
  | eq_num(One_more_than(x), Zero)             = false
  | eq_num(One_more_than(x), One_more_than(y)) = eq_num(x, y);

eq_num(Zero, One_more_than(Zero));
eq_num(One_more_than(Zero), One_more_than(Zero));
eq_num(One_more_than(Zero), One_more_than(One_more_than(Zero)));

fun eq_num(Zero, Zero)                         = true
  | eq_num(One_more_than(x), One_more_than(y)) = eq_num(x, y)
  | eq_num(x, y)                               = false;

eq_num(Zero, One_more_than(Zero));
eq_num(One_more_than(Zero), One_more_than(Zero));
eq_num(One_more_than(Zero), One_more_than(One_more_than(Zero)));

(* Output of code from this file
$ sml < 05-couples-are-magnificient-too.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- datatype 'a pizza = Bottom | Topping of 'a * 'a pizza
datatype fish = Anchovy | Lox | Tuna
val it = Bottom : 'a pizza
val it = fn : 'a * 'a pizza -> 'a pizza
val it = Topping (Anchovy,Bottom) : fish pizza
val it = Topping (Anchovy,Topping (Tuna,Topping #)) : fish pizza
val remove_anchovy = fn : fish pizza -> fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val remove_anchovy = fn : fish pizza -> fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val rem_fish = fn : fish * fish pizza -> fish pizza
val it = Topping (Tuna,Bottom) : fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val eq_fish = fn : fish * fish -> bool
val it = fn : fish * fish -> bool
val it = true : bool
val it = false : bool
val rem_fish = fn : fish * fish pizza -> fish pizza
val it = Topping (Tuna,Bottom) : fish pizza
val it = Topping (Lox,Topping (Tuna,Bottom)) : fish pizza
val eq_int = fn : int * int -> bool
val rem_int = fn : int * int pizza -> int pizza
val it = Topping (2,Bottom) : int pizza
val it = Topping (2,Topping (3,Bottom)) : int pizza
val subst_fish = fn : fish * fish * fish pizza -> fish pizza
val it = Topping (Lox,Topping (Lox,Topping #)) : fish pizza
datatype num = One_more_than of num | Zero
val eq_num = fn : num * num -> bool
val it = false : bool
val it = true : bool
val it = false : bool
val eq_num = fn : num * num -> bool
val it = false : bool
val it = true : bool
val it = false : bool
)