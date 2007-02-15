(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Type UnitTest
   val verifyNotFound =
       verifyFailsWith (fn CeeCache.NotFound => true | _ => false)
in
   unitTests
      (title "CeeCache")
      (test (fn () => let
                   open CeeCache
                   val c = new ()
                   val () = verifyTrue (0 = size c)
                   val k5 = put c 5
                   val () = verifyTrue (1 = size c)
                   val k2 = put c 2
                   val () = verifyTrue (2 = size c)
                   val () = verifyTrue (5 = use c k5)
                   val () = verifyNotFound (fn () => get c k5)
                   val () = verifyTrue (1 = size c)
                   val k3 = put c 3
                   val () = verifyTrue (2 = use c k2)
                   val () = verifyNotFound (fn () => use c k2)
                   val () = verifyTrue (1 = size c)
                   val () = verifyTrue (3 = use c k3)
                   val () = verifyNotFound (fn () => get c k3)
                   val () = verifyTrue (0 = size c)
                in
                   ()
                end))
      $
end
