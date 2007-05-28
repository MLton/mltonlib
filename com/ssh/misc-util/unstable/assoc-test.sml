(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Type UnitTest

   fun chkAssoc op + t =
       chk (all (t &` t &` t)
                (fn x & y & z =>
                    that (eq t ((x + y) + z, x + (y + z)))))
in
   unitTests
      (title "Assoc")

      (chkAssoc op + word)

      (chkAssoc op * real) (* This is supposed to fail. *)

      $
end
