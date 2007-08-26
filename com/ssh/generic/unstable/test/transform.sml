(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure Generic = struct
      open Generic
      local
         structure Open = WithTransform (Open)
         structure Extra = CloseWithExtra (Open)
      in
         open Open Extra
      end
   end

   open Generic UnitTest

   fun testTransform unOp t t2t value expect = let
      val transform = makeTransform unOp t t2t
   in
      testEq (t2t t) (fn () => {expect = expect, actual = transform value})
   end

   structure BinTree = MkBinTree (Generic)
in
   val () =
       unitTests
          (title "Generic.Transform")

          (testTransform (1 <\ op +) int list [1, 2, 3] [2, 3, 4])
          (testTransform op ~ int (fn t => tuple (T int *` T t)) (1 & 3) (1 & ~3))

          let
             datatype t = datatype BinTree.t
          in
             testTransform
                (1 <\ op +) int BinTree.t
                (BR (BR (LF, 0, LF), 1, BR (LF, 2, BR (LF, 3, LF))))
                (BR (BR (LF, 1, LF), 2, BR (LF, 3, BR (LF, 4, LF))))
          end

          $
end
