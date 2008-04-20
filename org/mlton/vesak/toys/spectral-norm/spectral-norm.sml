(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Array Cvt Iter

val op @ = Unsafe.Array.sub
val update = Unsafe.Array.update

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1

fun aij i j = 1.0 / Real.fromInt ((i+j) * (i+j+1) div 2 + (i+1))

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
