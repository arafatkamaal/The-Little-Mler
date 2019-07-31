(*
** New datatype
*)

datatype shish_kebab = 
    Skewer
  | Onion  of shish_kebab
  | Lamb   of shish_kebab
  | Tomato of shish_kebab;

(* What is an element of this new type
**   Answer: shish_kebab
*)
Onion;

(*
** What is an element of this new type
     Answer: shish_kebab
*)
Skewer;

(*
** What is an element of the below type
     Answer: shish_kebab
*)
Onion(
    Skewer);

(*
** Three levels of type definition
*)
Onion(
    Lamb(
        Onion(
            Skewer)));

(*
** Are there only onions in this
    Could be, this is a plane skewer
*)
Skewer;

(*
** What about?
*)
Onion(
    Skewer); (*yes*)

(*
** What about?
*)
Onion(
    Onion(
        Onion(
            Skewer))); (*yes*)

(*
** What about?
*)
Onion(
    Lamb(
        Onion(
            Skewer))); (*no*)

(*
** function that takes shish_kebab, and gives bool
*)
fun only_onions(Skewer)
    = true
  | only_onions(Onion(x))
    = only_onions(x)
  | only_onions(Lamb(x))
    = false
  | only_onions(Tomato(x))
    = false;

(*
**  fn : shish_kebab -> bool
*)
only_onions;

(*
** Testing some stuff now
*)
only_onions(Onion(
                Onion(
                    Onion(
                        Skewer))));

only_onions(Onion(
                Onion(
                    Lamb(
                        Skewer))));

only_onions(Skewer);

(*
** function that takes shish_kebab, and returns bool
*)
fun is_vegetarian(Skewer)
    = true
  | is_vegetarian(Onion(x))
    = is_vegetarian(x)
  | is_vegetarian(Lamb(x))
    = false
  | is_vegetarian(Tomato(x))
    = is_vegetarian(x);

is_vegetarian;

(*
** Testing some stuff now
*)
is_vegetarian(Onion(
                Onion(
                    Onion(
                        Skewer))));

is_vegetarian(Onion(
                Onion(
                    Lamb(
                        Skewer))));

is_vegetarian(Skewer);

(*
** Parameterize shish kebab from earlier
*)                       
datatype 'a shish =
    Bottom of 'a
  | Onion  of 'a shish
  | Lamb   of 'a shish
  | Tomato of 'a shish;

(*
** previous shish_kebab only worked on a skewer, this works on Bottom of anything
** Let's define that anything
*)
datatype rod =
    Dagger
  | Fork
  | Sword;

(*
** Some more bottom objects
*)
datatype plate =
    Gold_plate
  | Silver_plate
  | Brass_plate;

(*
** Let's see how the bottoms and shish fit
*)
Onion(
    Tomato(
        Bottom(
            Dagger))); (* This is a rod shish 
                            Onion (Tomato (Bottom Dagger)) : rod shish *)

Onion(
    Tomato(
        Bottom(
            Gold_plate))); (* This is a rod shish 
                                Onion (Tomato (Bottom Gold_plate)) : plate shish *)

(*
** Lets work on this function
*)
fun is_veggie(Bottom(x))
        = true
  | is_veggie(Onion(x))
        = is_veggie(x)
  | is_veggie(Lamb(x))
        = false
  | is_veggie(Tomato(x))
        = is_veggie(x); (* fn : 'a shish -> bool *)

is_veggie;

is_veggie(Onion(
            Tomato(
                Bottom(
                    Gold_plate))));

is_veggie(Onion(
            Tomato(
                Bottom(
                    Dagger))));

is_veggie(Onion(
            Lamb(
                Bottom(
                    Dagger))));
  
(*
** Whoa! Onion (Lamb (Bottom 52)) : int shish
** INT SHISH
*)
(Onion(
    Lamb(
        Bottom(
            52))));  
is_veggie(Onion(
            Lamb(
                Bottom(
                    52))));  

(*
** Redifining it here, just for testing
*)
datatype num =
    Zero
  | One_more_than of num;

val crazy_stuff = Onion(
                    Tomato(
                        Bottom(
                            One_more_than(
                                Zero)))); (*num shish*)

is_veggie(crazy_stuff);

(*Still works, bool shish*)
Onion(
    Tomato(
        Bottom(
            false)));

(*
** 'a Feels like catch all
*)

(*
** function to return to the bottom of what we have
** simple: fn : 'a shish -> 'a
*)
fun what_bottom(Bottom(x))
      = x
  | what_bottom(Onion(x))
      = what_bottom(x)
  | what_bottom(Lamb(x))
      = what_bottom(x)
  | what_bottom(Tomato(x))
      = what_bottom(x);

what_bottom;

what_bottom(Onion(
                Tomato(
                    Bottom(
                        false))));

what_bottom(Onion(
                Tomato(
                    Bottom(
                        One_more_than(
                            Zero))))); (* One_more_than Zero : num *)

(*
    Output of code in this file

$ sml < matchmaker-matchmaker.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- datatype shish_kebab
  = Lamb of shish_kebab
  | Onion of shish_kebab
  | Skewer
  | Tomato of shish_kebab
val it = fn : shish_kebab -> shish_kebab
val it = Skewer : shish_kebab
val it = Onion Skewer : shish_kebab
val it = Onion (Lamb (Onion Skewer)) : shish_kebab
val it = Skewer : shish_kebab
val it = Onion Skewer : shish_kebab
val it = Onion (Onion (Onion Skewer)) : shish_kebab
val it = Onion (Lamb (Onion Skewer)) : shish_kebab
val only_onions = fn : shish_kebab -> bool
val it = fn : shish_kebab -> bool
val it = true : bool
val it = false : bool
val it = true : bool
val is_vegetarian = fn : shish_kebab -> bool
val it = fn : shish_kebab -> bool
val it = true : bool
val it = false : bool
val it = true : bool
datatype 'a shish
  = Bottom of 'a | Lamb of 'a shish | Onion of 'a shish | Tomato of 'a shish
datatype rod = Dagger | Fork | Sword
datatype plate = Brass_plate | Gold_plate | Silver_plate
val it = Onion (Tomato (Bottom Dagger)) : rod shish
val it = Onion (Tomato (Bottom Gold_plate)) : plate shish
val is_veggie = fn : 'a shish -> bool
val it = fn : 'a shish -> bool
val it = true : bool
val it = true : bool
val it = false : bool
val it = Onion (Lamb (Bottom 52)) : int shish
val it = false : bool
datatype num = One_more_than of num | Zero
val crazy_stuff = Onion (Tomato (Bottom (One_more_than Zero))) : num shish
val it = true : bool
val it = Onion (Tomato (Bottom false)) : bool shish
val what_bottom = fn : 'a shish -> 'a
val it = fn : 'a shish -> 'a
val it = false : bool
val it = One_more_than Zero : num
- 


*)
