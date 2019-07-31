

(*Is this is a number: Yes
*)
5;

(*Both number and a integer: Yes
*)
17;

(*Test negative numbers
~23
*)
23;

(*This is of type boolean
*)
true;

(*Of type real
*)
5.32;

(*Lets define a new type
*)
datatype seasoning =
    Salt
  | Pepper;

(*Both are of type seasoning
*)
Salt;
Pepper;


(*First hint of a recursive type
*)
datatype num =
    Zero
  | One_more_than of num;

(*Zero is num
*)
Zero;

(* Constructs num from a num
*)
One_more_than;

(*is a num, because see above
*)
One_more_than(Zero);

(* is a num, because see above
*)
One_more_than(
  One_more_than(
    Zero));

(* is a num, because see above
*)
One_more_than(
  One_more_than(
    One_more_than(
      One_more_than(
        Zero))));

(* O is belongs type int
*)
0;

(*Zero belongs to type num
*)
Zero;

(* Nonsensical definiton because 
**0 is not Zero
**int not the same as num
*)
(*
One_more_than(0);
*)


(* new datatype with alpha definition
**This is apparently called a recursive, parameterized type
*)
datatype 'a open_faced_sandwich =
    Bread of 'a
  | Slice of 'a open_faced_sandwich;

(* Bread 0 : int open_faced_sandwich
** Element of 'a open_faced_sandwich
*)
Bread(0);

(* Bread true : bool open_faced_sandwich
** Element of 'a open_faced_sandwich
*)
Bread(true);

(*
** int open_faced_sandwich AND
** bool open_faced_sandwich
**    are not the same 
** That recursive, parameterized type is a fancy name for 
**    'Shape, that represents many datatypes'
*)

(* This gives an error, basically ungrammatical
**  But we get the idea why

datatype int open_faced_sandwich =
    Bread of int
  | Slice of int open_faced_sandwich;
*)

(* Instead of a rigid definition we can parameterize with the types we defined before
*)
Bread(Zero); (*num open_faced_sandwich*)
Bread(Salt); (*seasoning open_faced_sandwich*)

(*
**This now means we can derive as many types we want from the shape 'a open_faced_sandwich
*)

(* Let's try some more advanced defintions
*)

(* Bread (One_more_than Zero) : num open_faced_sandwich
*)
Bread(
  One_more_than(
    Zero));

(* A recursive type that belongs to itself
**     int open_faced_sandwich open_faced_sandwich
*)
Bread(
  Bread(0));

(*
** num open_faced_sandwich open_faced_sandwich
*)
Bread(
  Bread(
    One_more_than(
      Zero)));

(*
    Output of code int his file

$ sml < building-blocks.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- val it = 5 : int
val it = 17 : int
val it = 23 : int
val it = true : bool
val it = 5.32 : real
datatype seasoning = Pepper | Salt
val it = Salt : seasoning
val it = Pepper : seasoning
datatype num = One_more_than of num | Zero
val it = Zero : num
val it = fn : num -> num
val it = One_more_than Zero : num
val it = One_more_than (One_more_than Zero) : num
val it = One_more_than (One_more_than (One_more_than (One_more_than Zero)))
  : num
val it = 0 : int
val it = Zero : num
datatype 'a open_faced_sandwich
  = Bread of 'a | Slice of 'a open_faced_sandwich
val it = Bread 0 : int open_faced_sandwich
val it = Bread true : bool open_faced_sandwich
val it = Bread Zero : num open_faced_sandwich
val it = Bread Salt : seasoning open_faced_sandwich
val it = Bread (One_more_than Zero) : num open_faced_sandwich
val it = Bread (Bread 0) : int open_faced_sandwich open_faced_sandwich
val it = Bread (Bread (One_more_than Zero))
  : num open_faced_sandwich open_faced_sandwich

*)