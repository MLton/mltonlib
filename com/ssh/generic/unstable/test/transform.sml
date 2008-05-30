(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   fun testTransform t2t t unOp value expect =
       case makeTransform t2t t unOp
        of transform =>
           testEq (t2t t) (fn () => {expect = expect, actual = transform value})
in
   unitTests
    (title "Generic.Transform")

    (testTransform list int (1 <\ op +) [1, 2, 3] [2, 3, 4])
    (testTransform (fn t => tuple (T int *` T t)) int op ~ (1 & 3) (1 & ~3))

    let
       datatype t = datatype BinTree.t
    in
       testTransform
        BinTree.t int (1 <\ op +)
        (BR (BR (LF, 0, LF), 1, BR (LF, 2, BR (LF, 3, LF))))
        (BR (BR (LF, 1, LF), 2, BR (LF, 3, BR (LF, 4, LF))))
    end

    (testTransform Graph.t int op ~ Graph.intGraph1 Graph.intGraph1)

    $
end
