(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is basically an implementation of Jon Harrop's rewrite simplifier
 * toy benchmark.  See:
 *
 *   [http://groups.google.com/group/comp.lang.lisp/msg/a3ba5d7372a05917]
 *   [http://groups.google.com/group/comp.lang.functional/msg/75963bc5d77123b9]
 *)

(* Silly implementation of rational numbers
 *
 * HINT: Someone should really implement a nice lib for rational numbers!
 *)
datatype rational =
   INT of IntInf.t
 | //  of IntInf.t * IntInf.t

infix 7 */
infix 6 +/
infix //

fun gcd (a, b) : IntInf.t = if 0 = b then a else gcd (b, a mod b)

val normalize =
 fn 0 // _     => INT 0
  | r as INT _ => r
  | n // d     => let
       val c = gcd (n, d)
    in
       if c = d
       then INT (n div c)
       else n div c // d div c
    end

val op +/ = let
   fun sym i n d = n + i * d // d
in
   fn (INT l,   INT r) => INT (l + r)
    | (INT i,  n // d) => sym i n d
    | (n // d,  INT i) => sym i n d
    | (n // d, m // e) =>
      normalize (if d = e then n + m // d else n*e + m*d // d*e)
end

val op */ = let
   fun sym i n d = normalize (i*n // d)
in
   fn (INT l,   INT r) => INT (l * r)
    | (INT i,  n // d) => sym i n d
    | (n // d,  INT i) => sym i n d
    | (n // d, m // e) => normalize (n*m // d*e)
end

(* Expression datatype *)
datatype expr =
   NUM of rational
 | +`  of expr * expr
 | *`  of expr * expr
 | $   of String.t

infix 2 *`
infix 1 +`

(* Simplifier *)
infix 7 *:
infix 6 +:

val rec op +: =
 fn (NUM x,             NUM y) => NUM (x +/ y)
  | (NUM (INT 0),           x) => x
  | (x,           NUM (INT 0)) => x
  | (x,                y +` z) => x +: y +:z
  | other                      => op +` other

val rec op *: =
 fn (NUM x,                       NUM y) => NUM (x */ y)
  | (x as NUM (INT 0),                _) => x
  | (_,                y as NUM (INT 0)) => y
  | (NUM (INT 1),                     y) => y
  | (x,                     NUM (INT 1)) => x
  | (x,                          y *` z) => x *: y *: z
  | other                                => op *` other

val rec simplify =
 fn l +` r => simplify l +: simplify r
  | l *` r => simplify l *: simplify r
  | other  => other

(* Shorthand *)
val ` = NUM o INT

(* Naïve Benchmark
 *
 * NOTE: Seems not to be eliminated by MLton, but wouldn't count on it.
 * I would assume that the // constructor gets eliminated by MLton, but I
 * haven't verified this.
 *)
val expr = $"x" *` (`12 *` `0 +` (`23 +` `8) +` $"y")

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val () = repeat (fn () => ignore (simplify expr)) n ()
