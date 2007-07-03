(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is basically an implementation of the Cheap Concurrency toy
 * benchmark, from the ``Computer Language Benchmarks Game'', using a
 * library for portable asynchronous programming in SML.  The Async
 * library does not use threads or processes of any kind.  Measure the
 * performance yourself!
 *)

open Async

(* Makes a single handler that takes an integer message from a
 * channel, increments it, and gives it to another channel. *)
fun handler im = let
   val om = Ch.new ()
   fun lp () =
       when (Ch.take im)
            (fn x =>
                when (Ch.give om (x+1))
                     lp)
in
   lp () ; om
end

(* Makes a chain (head -> ... -> tail) of 500 handlers. *)
val head = Ch.new ()
val tail = repeat handler 500 head

(* An utility function for repeating an event a given number of times. *)
fun rept event combine finish n s =
    if n = 0
    then finish s
    else when event (fn x => rept event combine finish (n-1) (combine (x, s)))

(* Gets the number of times to pass a message through the chain. *)
val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1

(* Initiates a sequence of events that gives a 0 message to the head of
 * the chain n times and another sequence of events that takes a message
 * from the tail of the chain n times, accumulating them, and finally
 * printing the result.  Then runs all handlers to process the events. *)
val () =
    (rept (Ch.give head 0) ignore ignore n ()
   ; rept (Ch.take tail) op + (println o Int.toString) n 0
   ; Handler.runAll ())

(* PERFORMANCE ANALYSIS
 *
 *   This implementation spends a lot of time doing GC (> 40%) although
 *   the max live usage is small and stays roughly constant.  With a
 *   number of optimizations to the Async library, the GC time could
 *   probably be reduced significantly.
 *)
