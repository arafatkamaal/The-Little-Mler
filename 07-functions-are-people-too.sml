
(*
** Identity function consumes what it produces
*)
fun identity(x) = x;

(*
** True maker always produces true
*)
fun true_maker(x) = true;

(*New data type
*** constructors followed by 'of' are functions
*)
datatype bool_or_int =
    Hot  of bool
  | Cold of int;  

Hot(true); (*Hot true : bool_or_int*)
Cold(10);  (*Cold 10 : bool_or_int*)

(*
** fn : 'a -> (bool -> bool_or_int)
*)
fun hot_maker(x) = Hot;

(*
** ('a -> bool) -> bool_or_int
*)
fun help(f)
    = Hot(
        true_maker(
            if true_maker(5)
                then f
                else true_maker));

(*
** Self referential datatype
****  a. Take an int AND
****  b. Take a function that takes int, and produces chain
****  c. produce chain
*)
datatype chain =
    Link of (int * (int -> chain));

(* fn : int * (int -> chain) -> chain *)
Link;

(* fn : int -> chain
**  We need this because in the above point b, we need a function that takes in and gives a chain
*)
fun ints(n) = Link(n + 1, ints);

ints(0);

(* fn : int -> chain
** ints like function
*)
fun skips(n) = Link(n + 1, skips);

fun eq_int(n: int, m: int) = (n = m);
fun divides_evenly(n, c) = eq_int((n mod c), 0);
fun is_mod_5_or_7(n)
    = if divides_evenly(n, 5)
        then true
        else divides_evenly(n, 7);

fun some_ints(n)
    = if is_mod_5_or_7(n + 1)
        then Link(n + 1, some_ints)
        else some_ints(n + 1);

some_ints(116);

fun chain_item(n, Link(i, f))
    = if eq_int(n , 1)
        then i
        else chain_item(n - 1, f(i));

chain_item(1, some_ints(0));

(*
** Getting started on is_prime
*)

divides_evenly(10, 2);
divides_evenly(10, 3);

fun is_prime(n) = has_no_divisors(n, n - 1)
    and
    has_no_divisors(n, c) 
    =   if eq_int(c, 1)
            then true
            else
                if divides_evenly(n, c)
                    then false
                    else has_no_divisors(n, c - 1);

is_prime(10);
is_prime(9);
is_prime(13);

(*
** Chain of prime numbers 
*)
fun primes(n)
    = if is_prime(n + 1)
        then Link(n + 1, primes)
        else primes(n + 1);

primes(1);
primes(2);
primes(3);

chain_item(1, primes(1));
chain_item(2, primes(1));
chain_item(3, primes(1));
chain_item(4, primes(1));
chain_item(5, primes(1));
chain_item(6, primes(1));
chain_item(7, primes(1));
chain_item(12, primes(1));
chain_item(200, primes(1));
chain_item(2000, primes(1));
(*chain_item(20000, primes(1));*)

fun fibs(n)(m) = Link(n + m, fibs(m));
fun fibs_1(m)  = Link(1 + m, fibs(m));

fibs_1(1);
fibs_1(2);

(* Output of code from this file
$ sml < 07-functions-are-people-too.sml 
Standard ML of New Jersey v110.84 [built: Fri Sep 28 19:40:37 2018]
- val identity = fn : 'a -> 'a
val true_maker = fn : 'a -> bool
datatype bool_or_int = Cold of int | Hot of bool
val it = Hot true : bool_or_int
val it = Cold 10 : bool_or_int
val hot_maker = fn : 'a -> bool -> bool_or_int
val help = fn : ('a -> bool) -> bool_or_int
datatype chain = Link of int * (int -> chain)
val it = fn : int * (int -> chain) -> chain
val ints = fn : int -> chain
val it = Link (1,fn) : chain
val skips = fn : int -> chain
val eq_int = fn : int * int -> bool
val divides_evenly = fn : int * int -> bool
val is_mod_5_or_7 = fn : int -> bool
val some_ints = fn : int -> chain
val it = Link (119,fn) : chain
val chain_item = fn : int * chain -> int
val it = 5 : int
val it = true : bool
val it = false : bool
val is_prime = fn : int -> bool
val has_no_divisors = fn : int * int -> bool
val it = false : bool
val it = false : bool
val it = true : bool
val primes = fn : int -> chain
val it = Link (2,fn) : chain
val it = Link (3,fn) : chain
val it = Link (5,fn) : chain
val it = 2 : int
val it = 3 : int
val it = 5 : int
val it = 7 : int
val it = 11 : int
val it = 13 : int
val it = 17 : int
val it = 37 : int
val it = 1223 : int
val it = 17389 : int
val fibs = fn : int -> int -> chain
val fibs_1 = fn : int -> chain
val it = Link (2,fn) : chain
val it = Link (3,fn) : chain
*)