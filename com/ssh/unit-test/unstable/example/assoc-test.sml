(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This file contains a simple example of a QuickCheck -style
 * randomized test using the UnitTest framework.
 *)

val () = let
   open Generic UnitTest

   fun assoc op + t =
       all (t &` t &` t)
           (fn x & y & z =>
               that (eq t ((x + y) + z, x + (y + z))))
in
   unitTests
      (title "Assoc")

      (chk (assoc op + word))
      (* This law holds. *)

      (chk (assoc op + real))
      (* This law does not hold. *)

      $
end
