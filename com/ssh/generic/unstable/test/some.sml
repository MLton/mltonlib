(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   fun listEither swap mirror a =
       (Tie.fix Y)
        (fn aListLeft =>
            data' (op +` (swap (C0' "nil",
                                C1' "::" (tuple2 (a, aListLeft)))))
                  (mirror <--> (fn [] => INL () | op :: ? => INR ?,
                                fn INL () => [] | INR ? => op :: ?)))

   fun listL ? = listEither id   (id,     id)     ?
   fun listR ? = listEither swap (mirror, mirror) ?
in
   unitTests
    (title "Generic.Some")

    (* Test that generation terminates both ways. *)
    (testEq (list int)
            (fn () =>
                {actual = some (listL int),
                 expect = some (listR int)}))

    (testEq (BinTree.t int)
            (fn () =>
                {actual = some (BinTree.t int),
                 expect = BinTree.LF}))

    $
end
