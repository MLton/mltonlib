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

   fun thatAssoc op + t =
       all (t &` t &` t)
           (fn x & y & z =>
               thatEq t {actual = (x + y) + z,
                         expect = x + (y + z)})
in
   unitTests
      (title "Assoc")

      (test (fn () => thatAssoc op + word))
      (* This law holds. *)

      (test (fn () => thatAssoc op + real))
      (* This law does not hold. *)

      $
end
