(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Seq = struct

   open Seq

   fun unfold (b, f) =
      Util.recur
      (b, fn (b, loop) =>
       delay (fn () =>
              case f b of
                 None => empty ()
               | Some (a, b) => cons (a, loop b)))

   fun unfoldN (n, b, f) =
      unfold
      ((0, b), fn (i, b) =>
       if i = n then
          None
       else
          case f (i, b) of
             None => die "unfoldN"
           | Some (a, b) => Some (a, (i + 1, b)))

end
