(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Silly implementation of rational numbers
 *
 * HINT: Someone should really implement a nice lib for rational numbers!
 *)

infix 7 */
infix 6 +/
infix 0 //

datatype rat = INT of IntInf.t | // of IntInf.t Sq.t

val canon =
 fn 0 // _     => INT 0
  | r as INT _ => r
  | n // d     => let
       fun gcd (a, 0) = a
         | gcd (a, b) = gcd (b, a mod b)

       val c = gcd (n, d)
    in
       if c=d then INT (n div c) else n div c // d div c
    end

val op +/ = let
   fun sym i n d = n + i * d // d
in
   fn (INT l, INT r) => INT (l + r)
    | (INT i,  n//d) => sym i n d
    | (n//d,  INT i) => sym i n d
    | (n//d,   m//e) => canon (if d=e then n+m // d else n*e + m*d // d*e)
end

val op */ = let
   fun sym i n d = canon (i*n // d)
in
   fn (INT l, INT r) => INT (l * r)
    | (INT i,  n//d) => sym i n d
    | (n//d,  INT i) => sym i n d
    | (n//d,   m//e) => canon (n*m // d*e)
end
