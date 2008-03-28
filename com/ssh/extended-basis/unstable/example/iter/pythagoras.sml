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
 * One difference worth noting is that the below version returns an
 * iterator rather than a list.  Iterators and iterator combinators are
 * first-class values, while SRFI-42 style eager comprehensions are
 * second-class expressions.
 *
 * A less favorable observation is that the lack of custom notation for
 * monads makes the translation somewhat noisier than the SRFI-42 version.
 *)

open Cvt Iter

val sq = Int.sq
val op >> = Monad.>>
val guard = Monad.guard

fun pythagoras n =
    upTo (n+1) From 1 $ >>= (fn a =>
    upTo (n+1) From a $ >>= (fn b =>
    guard (sq a + sq b <= sq n) >>
    upTo (n+1) From b $ >>= (fn c =>
    guard (sq a + sq b = sq c) >>
    return (a, b, c))))

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val () =
    (pythagoras n)
     (fn (a, b, c) => printlns [D a, " ", D b, " ", D c])
