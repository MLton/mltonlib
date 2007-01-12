(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Unit tests for the {Misc} module.
 *)

val () = let
   open Type UnitTest
in
   unitTests
      (title "Misc.ranqd1")

      (testEq
          (list word32)
          (fn () =>
              {expect = [0wxCBF633B1, 0wx94F0AF1A, 0wx81FDBEE7,
                         0wxA3D95FA8, 0wx57FE6C2D, 0wx9F2EC686,
                         0wx6252E503, 0wxAAF95334, 0wxD1CCF6E9,
                         0wx47502932, 0wx3C6EF35F, 0wx0],
               actual = #2 |< repeat
                         (fn (x, ys) =>
                             (Misc.ranqd1 x, x::ys))
                         12
                         (0w0, [])}))

      (title "Misc.psdes")

      (testEq
          (list (sq word32))
          (fn () =>
              {expect = [(0wx604D1DCE, 0wx509C0C23),
                         (0wxD97F8571, 0wxA66CB41A),
                         (0wx7822309D, 0wx64300984),
                         (0wxD7F376F0, 0wx59BA89EB)],
               actual = map Misc.psdes
                            [(0w1, 0w1), (0w1, 0w99),
                             (0w99, 0w1), (0w99, 0w99)]}))

      $
end
