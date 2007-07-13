(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Shorthand *)
val ` = NUM o INT

(* Naïve Benchmark
 *
 * NOTES:
 * - Seems not to be eliminated by MLton, but wouldn't count on it.
 * - I would assume that the // constructor or rational gets eliminated by
 *   MLton, but I haven't verified this.
 *)
val expr = $"x" *` (`12 *` `0 +` (`23 +` `8) +` $"y")

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val () = repeat (fn () => ignore (simplify expr)) n ()
