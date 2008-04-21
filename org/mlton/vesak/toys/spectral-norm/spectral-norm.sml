(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is an implementation of the Spectral Norm toy benchmark from the
 * ``Computer Language Benchmarks Game'' (TheGame).  Compared to the
 * SML/MLton version of the benchmark in TheGame at the time of writing,
 * this one reuses a part of the logic, namely one of the multiplication
 * functions, by using a higher-order function.  This version also makes
 * use of a library of iterator combinators rather than a hand written
 * tail recursive functions or a special purpose for-function.
 * Nevertheless, performance stays the same, because MLton specializes the
 * code, eliminating the abstraction penalty.  As a result, this
 * implementation is significantly shorter than the implementation in
 * TheGame.
 *
 * This benchmark is clearly compute bound.  Disabling array bounds and
 * integer overflow checking
 *
 *   -const 'MLton.safe false'
 *   -const 'MLton.detectOverflow false'
 *
 * improves performance a little---although this is probably processor
 * dependent, because the inner loops are shortened quite a bit.  It would
 * seem that the main bottleneck is the floating point divide, which is an
 * exceptionally costly instruction (long latency and unpipelined) on most
 * processors.
 *)

open Array Cvt Iter

val op @ = sub

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1

fun aij i j = 1.0 / real ((i+j) * (i+j+1) div 2 + (i+1))

fun timesAv aij u v =
    upTo n $ (fn i =>
      update (v, i, reduce 0.0 op + (fn j => aij j i * (u@j)) (upTo n $)))

fun timesAtA u v =
    case array (n, 0.0) of w => (timesAv aij u w ; timesAv (flip aij) w v)

val u & v = array (n, 1.0) & array (n, 0.0)

val () =
    (upTo 10 $ (fn _ => (timesAtA u v ; timesAtA v u))
   ; (println o R#F 9 o Math.sqrt o op /)
      (fold (fn (i, (vBv, vv)) => (vBv + (u@i) * (v@i), vv + Real.sq (v@i)))
            (0.0, 0.0)
            (upTo n $)))
