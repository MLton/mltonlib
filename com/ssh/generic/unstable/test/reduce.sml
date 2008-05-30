(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic UnitTest

   fun testReduce t2t fromT toT zero binOp to value expect = let
      val reduce = makeReduce t2t fromT zero binOp to
   in
      testEq toT (fn () => {expect = expect, actual = reduce value})
   end

   structure Set = struct
      val empty = []
      fun singleton x = [x]
      fun union (xs, ys) = List.nubByEq op = (xs @ ys)
      fun difference (xs, ys) = List.filter (not o List.contains ys) xs
   end

   local
      open Set Lambda
      val refs = fn REF id => singleton id | _ => empty
      val decs = fn FUN (id, _) => singleton id | _ => empty
   in
      fun free (IN term) =
          difference
           (union (refs term,
                   makeReduce f t empty union free term),
            decs term)
   end
in
   val () =
       unitTests
        (title "Generic.Reduce")

        (testReduce list int int 0 op + id [1, 2, 3] 6)
        (testReduce list real int 0 op + (const 1) [1.0, 4.0, 6.0] 3)
        (testReduce (fn t => tuple (T t *` T int *` T t)) int int 0 op + id
                    (1 & 3 & 7) 8)

        let open BinTree in
           testReduce t int (list int) [] op @ (fn x => [x])
                      (BR (BR (LF, 0, LF), 1, BR (LF, 2, BR (LF, 3, LF))))
                      [0, 1, 2, 3]
        end

        (testEq (list string)
                (fn () => let
                       open Lambda
                       fun ` f = IN o f
                    in
                       {actual = free (`APP (`FUN ("x",
                                                   `APP (`REF "y", `REF "x")),
                                             `FUN ("z",
                                                   `APP (`REF "x",
                                                         `APP (`REF "y",
                                                               `REF "x"))))),
                        expect = ["y", "x"]}
                    end))

        $
end
