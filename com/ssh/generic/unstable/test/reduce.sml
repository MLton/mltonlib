(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure Generic = struct
      open Generic
      local
         structure Open = WithReduce (Open)
         structure Extra = CloseWithExtra (Open)
      in
         open Open Extra
      end
   end

   open Generic UnitTest

   fun testReduce zero binOp to fromT t2t toT value expect = let
      val reduce = makeReduce zero binOp to fromT t2t
   in
      testEq toT (fn () => {expect = expect, actual = reduce value})
   end
in
   val () =
       unitTests
          (title "Generic.Reduce")

          (testReduce 0 op + id int list int [1, 2, 3] 6)
          (testReduce 0 op + (const 1) real list int [1.0, 4.0, 6.0] 3)
          (testReduce 0 op + id int (fn t => tuple (T t *` T int *` T t)) int
                      (1 & 3 & 7) 8)

          $
end
