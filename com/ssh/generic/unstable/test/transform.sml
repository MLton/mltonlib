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
         val makeTransform = Open.makeTransform
         open Extra
      end
   end

   open Generic UnitTest

   fun testTransform unOp t t2t value expect = let
      val transform = makeTransform unOp t t2t
   in
      testEq (t2t t) (fn () => {expect = expect, actual = transform value})
   end
in
   val () =
       unitTests
          (title "Generic.Transform")

          (testTransform (1 <\ op +) int list [1, 2, 3] [2, 3, 4])
          (testTransform op ~ int (fn t => tuple (T int *` T t)) (1 & 3) (1 & ~3))

          $
end
