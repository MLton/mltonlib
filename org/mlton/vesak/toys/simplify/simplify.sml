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

infix 7 *:
infix 6 +:

infix 2 *`
infix 1 +`

(* Expression datatype *)
datatype expr = NUM of rat | +` of expr Sq.t | *` of expr Sq.t | $ of String.t

(* Simplifier *)
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
