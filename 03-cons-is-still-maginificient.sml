
(*
** New pizza datatype
*)
datatype pizza =
    Crust
  | Cheese  of pizza
  | Onion   of pizza
  | Anchovy of pizza
  | Sausage of pizza;

Crust;

(*
** Anchovy (Onion (Anchovy (Anchovy (Cheese #)))) : pizza
*)
val salty_pizza = Anchovy(
                    Onion(
                      Anchovy(
                        Anchovy(
                          Cheese(
                            Crust)))));

(*
** Sausage (Onion (Anchovy (Sausage (Cheese #)))) : pizza
*)
val salty_sausage_pizza = Sausage(
                            Onion(
                              Anchovy(
                                Sausage(
                                  Cheese(
                                    Crust)))));


(*
** Making pizza less salty
*)
fun remove_anchovy(Crust)
      = Crust
  | remove_anchovy(Cheese(x))
      = Cheese(
          remove_anchovy(x))
  | remove_anchovy(Onion(x))
      = Onion(
          remove_anchovy(x))
  | remove_anchovy(Anchovy(x))
      = remove_anchovy(x)
  | remove_anchovy(Sausage(x))
      = Sausage(
          remove_anchovy(x)); (*fn : pizza -> pizza*)

remove_anchovy;

(*
** Sausage (Onion (Sausage (Cheese Crust))) : pizza
*)
remove_anchovy(salty_sausage_pizza);

(*
** Onion (Cheese Crust) : pizza
*)
remove_anchovy(salty_pizza); (*Seems to work so far*)

(*
** Making pizza greasy and salty
*)
fun top_anchovy_with_cheese(Crust)
      = Crust
  | top_anchovy_with_cheese(Cheese(x))
      = Cheese(
          top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Onion(x))
      = Onion(
          top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Anchovy(x))
      = Cheese(
          Anchovy(
            top_anchovy_with_cheese(x)))
  | top_anchovy_with_cheese(Sausage (x))
      = Sausage(
          top_anchovy_with_cheese(x));

top_anchovy_with_cheese;

(*Works
** Onion (Cheese (Anchovy (Cheese (Cheese #)))) : pizza
*)
top_anchovy_with_cheese(Onion(
                          Anchovy(
                            Cheese(
                              Anchovy(
                                Crust)))));

(*
** Works so wonderfully fine Onion (Cheese (Sausage Crust)) : pizza
*)
top_anchovy_with_cheese(Onion(
                          Cheese(
                            Sausage(
                              Crust))));

(*
**Onion (Cheese Crust) : pizza
*)
top_anchovy_with_cheese(
  remove_anchovy(
    Onion(
      Anchovy(
        Cheese(
          Anchovy(
            Crust))))));

(*
** Onion (Cheese (Cheese (Cheese Crust))) : pizza
*)
remove_anchovy(
  top_anchovy_with_cheese(
    Onion(
      Anchovy(
        Cheese(
          Anchovy(
            Crust))))));

fun subst_anchovy_with_cheese(x)
  = remove_anchovy(
      top_anchovy_with_cheese(x));            

fun subst_anchovy_with_cheese(Crust)
      = Crust
  | subst_anchovy_with_cheese(Cheese(x))
      = Cheese(subst_anchovy_with_cheese(x))
  | subst_anchovy_with_cheese(Onion(x))
      = Onion(subst_anchovy_with_cheese(x))
  | subst_anchovy_with_cheese(Anchovy(x))
      = Cheese(subst_anchovy_with_cheese(x))
  | subst_anchovy_with_cheese(Sausage(x))
      = Sausage(subst_anchovy_with_cheese(x));

(*Works*)
subst_anchovy_with_cheese(
    Onion(
      Anchovy(
        Cheese(
          Anchovy(
            Crust)))));

(*
    Output of code in this file

$ sml < 03-cons-is-still-maginificient.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- datatype pizza
  = Anchovy of pizza
  | Cheese of pizza
  | Crust
  | Onion of pizza
  | Sausage of pizza
val it = Crust : pizza
val salty_pizza = Anchovy (Onion (Anchovy (Anchovy (Cheese #)))) : pizza
val salty_sausage_pizza = Sausage (Onion (Anchovy (Sausage (Cheese #))))
  : pizza
val remove_anchovy = fn : pizza -> pizza
val it = fn : pizza -> pizza
val it = Sausage (Onion (Sausage (Cheese Crust))) : pizza
val it = Onion (Cheese Crust) : pizza
val top_anchovy_with_cheese = fn : pizza -> pizza
val it = fn : pizza -> pizza
val it = Onion (Cheese (Anchovy (Cheese (Cheese #)))) : pizza
val it = Onion (Cheese (Sausage Crust)) : pizza
val it = Onion (Cheese Crust) : pizza
val it = Onion (Cheese (Cheese (Cheese Crust))) : pizza
val subst_anchovy_with_cheese = fn : pizza -> pizza
val subst_anchovy_with_cheese = fn : pizza -> pizza
val it = Onion (Cheese (Cheese (Cheese Crust))) : pizza

*)