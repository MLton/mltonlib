(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is basically an implementation of the Cheap Concurrency toy
 * benchmark, from the ``Computer Language Benchmarks Game'', using a
 * library for portable asynchronous programming in SML.  This
 * implementation was inspired by a Haskell implementation by Einar
 * Karttunen, Simon Marlow, and Don Stewart.  The Async library does not
 * use threads or processes of any kind.  Measure the performance
 * yourself!
 *)

open Async

fun handler im = let
   val om = MVar.new ()
in
   every (MVar.take im) (fn x => MVar.fill om (x+1))
 ; om
end

val head = MVar.new ()
val tail = repeat handler 500 head

fun accumulate n sum =
    if n = 0
    then println (Int.toString sum)
    else (MVar.fill head sum
        ; when (MVar.take tail) (accumulate (n-1)))

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1

val () = (accumulate n 0 ; Handler.runAll ())
