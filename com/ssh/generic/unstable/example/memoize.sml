(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This example shows a simple way to implement memoization using the
 * generics library.
 *
 * The {memo} function is given a type representation for the type of the
 * domain of a function to memoize and a function.  It then returns a
 * memoized function.  The memoized function uses a hash table to map
 * values of the domain to values of the codomain.
 *
 * The {loud} function is given the type representations of both the
 * domain and codomain of a function (and a name and a function) and it
 * then returns a function that prints calls to the function.  This is
 * used to show that the memo function works.
 *
 * The {test} function is similar.  It fails with a pretty printed
 * function call with the actual and expected results in case the actual
 * and expected results disagree.
 *
 * Note that the example code here is not powerful enough to memoize
 * recursive functions.  See
 *
 *   That About Wraps it Up: Using FIX to Handle Errors Without
 *     Exceptions, and Other Programming Tricks
 *   Bruce J. McAdam
 *   [http://citeseer.ist.psu.edu/51062.html]
 *   [http://www.lfcs.inf.ed.ac.uk/reports/97/ECS-LFCS-97-375/]
 *
 * for a more general approach.
 *)

open Generic

fun memo dom f =
    case HashMap.new {eq = eq dom, hash = hash dom}
     of x2y =>
        fn x =>
           case HashMap.find x2y x
            of SOME y => y
             | NONE => case f x of y => (HashMap.insert x2y (x, y) ; y)

fun loud dom cod name f x =
    try (fn () => f x,
         fn y => (printlns [name, " ", show dom x, " = ", show cod y]
                ; y),
         fn e => (printlns [name, " ", show dom x, " = raise ", show exn e]
                ; raise e))

fun test dom cod name f x y =
    case f x
     of y' => if eq cod (y', y) then ()
              else fails [name, " ", show dom x, " = ", show cod y',
                          ", expected ", show cod y]

val concatV = concat o Vector.toList

val concatV = let
   val dom = vector string
   val cod = string
in
   loud dom cod "(memo) concatV" |< memo dom |< loud dom cod "concatV" concatV
end

val testConcatV = test (vector string) string "concatV" concatV

val () =
    (testConcatV (Vector.fromList ["a", "bcd"])     "abcd"
   ; testConcatV (Vector.fromList ["Ab", "C", "d"]) "AbCd"
   ; testConcatV (Vector.fromList ["a", "bcd"])     "abcd")
