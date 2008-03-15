(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest
in
   unitTests
      (title "Reverse (incorrect)")

      (testAll (sq (list int))
               (fn (xs, ys) =>
                   thatEq (list int)
                          {expect = rev (xs @ ys),
                           actual = rev xs @ rev ys}))

      (* Read the above as:
       *
       *   "Test for all pairs of lists of integers xs and ys, that the
       *    lists of integers expect and actual are equal, where the
       *    expect list is the reverse of the concatenation of xs and ys
       *    and the actual list is the concatenation of the reverse of xs
       *    and the reverse of ys."
       *
       * (Of course, this property does not hold.)
       *)

      $
end
