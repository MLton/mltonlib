(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Unit tests for the {Prettier} module.
 *)

val () = let
   open Type UnitTest Prettier

   datatype tree = N of String.t * tree List.t

   local
      fun tree (N (s, [])) = txt s
        | tree (N (s, ts)) =
          group (txt s <^> nest (1+size s) (brackets (trees ts)))
      and trees [] = empty
        | trees [t] = tree t
        | trees (t::ts) = tree t <^> comma <$> trees ts
   in
      val layoutTree = tree
   end

   val aTree =
       N ("aaa", [N ("bbbbb", [N ("ccc", []), N ("dd", [])]),
                  N ("eee", []),
                  N ("ffff", [N ("gg", []), N ("hhh", []), N ("ii", [])])])
in
   unitTests
      (title "Prettier")

      (testEq string
              (fn () =>
                  {expect = "this is\n\
                            \level one\n\
                            \text\n\
                            \  some\n\
                            \  level\n\
                            \  two text\n\
                            \    level\n\
                            \    three\n\
                            \    text\n\
                            \  level\n\
                            \  two text\n\
                            \  again",
                   actual = pretty (SOME 10)
                                   (str "this is level one text\n\
                                        \  some level two text\n\
                                        \    level three text\n\
                                        \  level two text again")}))
      (testEq string
              (fn () =>
                  {expect = "aaa[bbbbb[ccc, dd],\n\
                            \    eee,\n\
                            \    ffff[gg, hhh, ii]]",
                   actual = pretty (SOME 30) (layoutTree aTree)}))

      $
end
