(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is basically an example from
 *
 *   Eager Comprehensions in Scheme: The design of SRFI-42
 *   Sebastian Egner
 *   [http://library.readscheme.org/servlets/cite.ss?pattern=sw2005-Egner]
 *
 * translated to SML using iterator combinators.
 *
 * If you compare the SRFI-42 example from the above paper with the below
 * translation, notice that the SRFI-42 version repeats the lines
 *
 *> (:range k 2 n)
 *> (if (char=? (string-ref p k) #\1))
 *
 * twice, while the below version using iterator combinators defines the
 * iterator
 *
 *> val primes = filter isPrime (upTo n From 2 $)
 *
 * and uses it twice.  Also worth noting is that the below version returns
 * an iterator rather than a list.  Iterators and iterator combinators are
 * first-class values, while SRFI-42 style eager comprehensions are
 * second-class expressions.
 *)

open Cvt Iter

fun eratosthenes n = let
   val p = Array.array (n, true)
   fun isPrime i = Array.sub (p, i)
   val primes = filter isPrime (upTo n From 2 $)
in
   (primes >>= (fn k => upTo n From (2*k) By k $))
    (fn i => Array.update (p, i, false))
 ; primes
end

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val () = eratosthenes n (println o D)
