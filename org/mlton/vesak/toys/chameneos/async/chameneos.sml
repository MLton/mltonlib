(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is basically a translation of a Chameneos toy benchmark
 * implementation by Tom Pledger for Haskell, from the Computer Language
 * Benchmarks Game, using a library for portable asynchronous programming
 * in SML.  The Async library does not use threads or processes of any
 * kind.  Measure the performance yourself!
 *)

open Async

datatype color = R | B | Y

val compl =
 fn B&B => B | B&R => Y | B&Y => R
  | R&B => Y | R&R => R | R&Y => B
  | Y&B => R | Y&R => B | Y&Y => Y

val mp = MVar.new ()
val wake = MVar.new ()

val subCols = [B, R, Y]

fun arrive tally color =
    when (MVar.take mp)
         (fn {quota = 0, done = d, waiter = w} =>
             if length d = length subCols
             then println (Int.toString (foldl op + tally d))
             else MVar.fill mp {quota = 0, done = tally::d, waiter = w}
           | {waiter = NONE, done = d, quota = q} =>
             (MVar.fill mp {waiter = SOME color, done = d, quota = q}
            ; when (MVar.take wake) (arrive (tally+1)))
           | {quota = q, waiter = SOME color0, done = d} => let
                val color = compl (color & color0)
             in MVar.fill wake color
              ; MVar.fill mp {quota = q-1, waiter = NONE, done = d}
              ; arrive (tally+1) color
             end)

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1

val () =
    (MVar.fill mp {quota = n, waiter = NONE, done = []}
   ; app (arrive 0) subCols
   ; arrive 0 B
   ; Handler.runAll ())
