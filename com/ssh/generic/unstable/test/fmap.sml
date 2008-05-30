(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic UnitTest

   structure ListF = MkFmap (open Generic List val t = list)
   structure BinTreeF = MkFmap (open Generic BinTree)
in
   val () =
       unitTests
        (title "Generic.Fmap")

        (testEq (list word)
                (fn () =>
                    {expect = [0w1, 0w2, 0w3],
                     actual = ListF.map Word.fromInt [1, 2, 3]}))

        let
           open BinTree BinTreeF
        in
           testEq (t word)
                  (fn () =>
                      {expect = BR (BR (LF, 0w0, LF),
                                    0w1,
                                    BR (LF, 0w2, BR (LF, 0w3, LF))),
                       actual = map Word.fromInt
                                    (BR (BR (LF, 0, LF),
                                         1,
                                         BR (LF, 2, BR (LF, 3, LF))))})
        end

        $
end
