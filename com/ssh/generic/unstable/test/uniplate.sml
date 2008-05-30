(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   fun testUniplate t =
       testAll t (fn x =>
          case uniplate t x
           of (c, c2x) =>
              (thatEq t {expect = x, actual = c2x c}
             ; thatEq (list t) {expect = c, actual = children t x}))

   fun testFoldU t =
       testAll t (fn x =>
          thatEq (list t)
                 {expect = universe t x,
                  actual = rev (foldU t op :: [] x)})

   fun testRewrite t f =
       testAll t (fn x =>
          app (fn x =>
                  thatEq (option t)
                         {expect = NONE,
                          actual = f x})
              (universe t (rewrite t f x)))

   fun testHolesU t =
       testAll t (fn x =>
          (thatEq (list t)
                  {expect = universe t x,
                   actual = map #1 (holesU t x)}
         ; app (fn (y, y2x) =>
                   thatEq t {expect = x,
                             actual = y2x y})
               (holesU t x)))
in
   unitTests
    (title "Generic.Uniplate")

    (testUniplate (BinTree.t int))
    (testUniplate (list int))

    (title "Generic.Uniplate.foldU")

    (testFoldU (BinTree.t int))
    (testFoldU (list int))

    (title "Generic.Uniplate.rewrite")

    let
       open BinTree
       val tryL =
        fn BR (BR (a, x, b), y, r) =>
           if y < x then SOME (BR (BR (a, y, b), x, r)) else NONE
         | _ => NONE
       val tryR =
        fn BR (l, y, BR (c, z, d)) =>
           if z < y then SOME (BR (l, z, BR (c, y, d))) else NONE
         | _ => NONE
    in
       testRewrite
        (t int)
        (fn x => case tryL x of NONE => tryR x | some => some)
    end

    (testRewrite (list int)
                 (fn x::y::r => if y < x then SOME (y::x::r) else NONE
                   | _       => NONE))

    (title "Generic.Uniplate.holesU")

    (testHolesU (BinTree.t int))
    (testHolesU (list int))

    $
end
