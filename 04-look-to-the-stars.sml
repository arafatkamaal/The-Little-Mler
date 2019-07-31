

(*
** Lets put up a good spread
*)

datatype meza =
    Shrimp
  | Calamari
  | Escargots
  | Hummus;

Shrimp;

datatype main =
    Steak
  | Ravioli
  | Chicken
  | Eggplant;

Eggplant;

datatype salad =
    Green
  | Cucumber
  | Greek;

Cucumber;

datatype dessert =
    Sundae
  | Mousse
  | Torte;

Mousse;

(*
** Meal prep 
**  (Calamari,Ravioli,Greek,Sundae) : meza * main * salad * dessert
*)

(Calamari, Ravioli, Greek, Sundae);

(*More meals*)
(Hummus,   Steak,   Green, Torte);
(*In this case eat desserts before main course*)
(Torte,    Hummus,  Steak, Sundae);

(*Small meal
** (Shrimp,Sundae) : meza * dessert
*)
(Shrimp, Sundae);

(*
** Add a steak
**    fn : 'a -> 'a * main
*)
fun add_a_steak(x) = (x, Steak);

add_a_steak(Shrimp);
add_a_steak(Hummus);

(*
** Gives non-sensical results, we can't eat integers, can we?
**    (int * meza) * main
*)
add_a_steak(5, Shrimp);

(*
** A little more detail
**    fn : meza -> meza * main
*)
fun add_a_steak(Shrimp)    = (Shrimp,    Steak)
  | add_a_steak(Calamari)  = (Calamari,  Steak)
  | add_a_steak(Escargots) = (Escargots, Steak)
  | add_a_steak(Hummus)    = (Hummus,    Steak);

add_a_steak(Shrimp);
add_a_steak(Hummus);

(*
** function to see if two main dishes are same
**    fn : main * main -> bool
*)
fun eq_main(Steak, Steak)      = true
  | eq_main(Steak, Ravioli)    = false
  | eq_main(Steak, Chicken)    = false
  | eq_main(Steak, Eggplant)   = false
  | eq_main(Ravioli, Steak)    = false
  | eq_main(Ravioli, Ravioli)  = true
  | eq_main(Ravioli, Chicken)  = false
  | eq_main(Ravioli, Eggplant) = false
  | eq_main(Chicken, Steak)    = false
  | eq_main(Chicken, Ravioli)  = false
  | eq_main(Chicken, Chicken)  = true
  | eq_main(Chicken, Eggplant) = false
  | eq_main(Eggplant, Steak)   = false
  | eq_main(Eggplant, Ravioli) = false
  | eq_main(Eggplant, Chicken) = false
  | eq_main(Eggplant, Eggplant)= true;

eq_main(Steak, Eggplant);

(*
** Shorten this
*)
fun eq_main(Steak, Steak)           = true
  | eq_main(Ravioli, Ravioli)       = true
  | eq_main(Chicken, Chicken)       = true
  | eq_main(Eggplant, Eggplant)     = true
  | eq_main(one_dish, another_dish) = false;

eq_main(Steak, Eggplant);
eq_main(Eggplant, Eggplant);

(*
** has steak, this should be easy, lets try
*)

fun has_steak(Steak, second, third)  = true
  | has_steak(first, Steak,  third)  = true
  | has_steak(first, second, Steak)  = true
  | has_steak(first, second, third)  = false;

has_steak(Steak, Steak, Chicken);
has_steak(Ravioli, Steak, Chicken);
has_steak(Ravioli, Eggplant, Chicken);

fun has_steak(a_meza, Steak,  a_dessert) = true
  | has_steak(a_meza, a_main, a_dessert) = false;

(*
** Ice creams are now allowed.
*)
has_steak(Steak, Steak, Chicken);
has_steak(Calamari, Steak, Chicken);
has_steak(Ravioli, Eggplant, Sundae);

(*
** Looks like garbage is also allowed
**  We can't eat an integer, can we?
*)  
has_steak(5, Steak, Chicken);

(*
** Let's fix this
*)
fun has_steak(a: meza, Steak,   c: dessert): bool
    = true
  | has_steak(a: meza, b: main, c: dessert): bool
    = false;

has_steak;

has_steak(Calamari, Steak,   Sundae);
has_steak(Calamari, Chicken, Torte);

(*
** Using this new thing we can add all kinds of shortening
**    fn : meza -> main * meza
*)
fun add_a_steak(x: meza): (main * meza)
    = (Steak, x);

add_a_steak(Calamari);

(* Output of code from this file

$ sml < 04-look-to-the-stars.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- datatype meza = Calamari | Escargots | Hummus | Shrimp
val it = Shrimp : meza
datatype main = Chicken | Eggplant | Ravioli | Steak
val it = Eggplant : main
datatype salad = Cucumber | Greek | Green
val it = Cucumber : salad
datatype dessert = Mousse | Sundae | Torte
val it = Mousse : dessert
val it = (Calamari,Ravioli,Greek,Sundae) : meza * main * salad * dessert
val it = (Hummus,Steak,Green,Torte) : meza * main * salad * dessert
val it = (Torte,Hummus,Steak,Sundae) : dessert * meza * main * dessert
val it = (Shrimp,Sundae) : meza * dessert
val add_a_steak = fn : 'a -> 'a * main
val it = (Shrimp,Steak) : meza * main
val it = (Hummus,Steak) : meza * main
val it = ((5,Shrimp),Steak) : (int * meza) * main
val add_a_steak = fn : meza -> meza * main
val it = (Shrimp,Steak) : meza * main
val it = (Hummus,Steak) : meza * main
val eq_main = fn : main * main -> bool
val it = false : bool
val eq_main = fn : main * main -> bool
val it = false : bool
val it = true : bool
val has_steak = fn : main * main * main -> bool
val it = true : bool
val it = true : bool
val it = false : bool
val has_steak = fn : 'a * main * 'b -> bool
val it = true : bool
val it = true : bool
val it = false : bool
val it = true : bool
val has_steak = fn : meza * main * dessert -> bool
val it = fn : meza * main * dessert -> bool
val it = true : bool
val it = false : bool
val add_a_steak = fn : meza -> main * meza
val it = (Steak,Calamari) : main * meza

*)