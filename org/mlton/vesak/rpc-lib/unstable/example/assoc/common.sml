(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

fun mkLib {bind, bindings, find} =
    {bind = bind (Pair.t (String.t, Int.t), Unit.t, "bind"),
     bindings =
      bindings (Unit.t, List.t (Pair.t (String.t, Int.t)), "bindings"),
     find = find (String.t, Option.t Int.t, "find")}

fun verbose h (d, c, n) f x =
    try (fn () => f x,
         fn y =>
            (printlns [h, n, " ", Generic.show d x, " => ", Generic.show c y]
           ; y),
         fn e =>
            (printlns
              [h, n, " ", Generic.show d x, " raised ", Generic.show Exn.t e]
           ; raise e))
