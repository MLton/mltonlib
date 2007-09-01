(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic UnitTest

   fun listEither pair sumIn sumOut a =
       (Tie.fix Y)
          (fn aListLeft =>
              iso (data (op +` (pair (C0' "nil",
                                      C1' "::" (tuple2 (a, aListLeft))))))
                  (sumIn o (fn [] => INL () | op :: ? => INR ?),
                   (fn INL () => [] | INR ? => op :: ?) o sumOut))

   fun listL ? = listEither id        id       id       ?
   fun listR ? = listEither Pair.swap Sum.swap Sum.swap ?

   structure BinTree = MkBinTree (Generic)
in
   val () =
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
