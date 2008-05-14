(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is an example from
 *
 *   http://article.gmane.org/gmane.comp.lang.haskell.cafe/39806
 *
 * translated to SML using iterator combinators.
 *
 * The mean computation runs in constant space.  The sequence of real
 * numbers is generated twice: first to compute the sum and then to
 * compute the length.  Of course, one could also compute the sum and
 * length simultaneously and only generate the sequence once.
 *)

open Cvt Iter

fun length xs = reduce 0 op + (const 1) xs
val sum = reduce 0.0 op + id
fun mean xs = sum xs / real (length xs)

val n = valOf (Real.fromString (hd (CommandLine.arguments ()))) handle _ => 1e9

val () = println (G (mean (realsTo n From 1.0 $)))
