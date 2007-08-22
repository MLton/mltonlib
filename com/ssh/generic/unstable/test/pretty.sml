(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   infix |`

   fun tst n t s v =
       testEq string (fn () => {expect = s, actual = pretty n t v})
in
   unitTests
      (title "Generic.Pretty")

      (tst NONE unit "()" ())

      (tst NONE word "0wx15" 0wx15)

      (tst (SOME 6) (list int)
           "[1,\n 2,\n 3]"
           [1, 2, 3])

      (tst (SOME 2) (vector bool)
           "#[true,\n\
           \  false]"
           (Vector.fromList [true, false]))

      (tst (SOME 15) (tuple3 (option unit, string, exn))
           "(NONE,\n\
           \ \"a\",\n\
           \ Empty)"
           (NONE, "a", Empty))

      (tst NONE (array unit) "#()" (Array.array (0, ())))

      (tst NONE real "~3.141" ~3.141)

      (tst (SOME 22)
           ((order |` unit) &` order &` (unit |` order))
           "&\n\
           \ (& (INL LESS, EQUAL),\n\
           \  INR GREATER)"
           (INL LESS & EQUAL & INR GREATER))

      let
         fun chk s e = tst (SOME 11) string e s
      in
         fn ? =>
            (pass ?)
               (chk "does not fit"   "\"does not fit\"")
               (chk "does\nnot\nfit" "\"does\\n\\\n\\not\\n\\\n\\fit\"")
               (chk "does fit"       "\"does fit\"")
               (chk "does\nfit"      "\"does\\nfit\"")
      end

      let
         exception Unknown
      in
         tst NONE exn "#Unknown" Unknown
      end

      (tst (SOME 9)
           (iso (record (R' "1" int
                      *` R' "+" (unOp int)
                      *` R' "c" char))
                (fn {1 = a, + = b, c = c} => a & b & c,
                 fn a & b & c => {1 = a, + = b, c = c}))
           "{1 = 2,\n\
           \ + = #fn,\n\
           \ c =\n\
           \  #\"d\"}"
           {1 = 2, + = id, c = #"d"})

      let
         datatype s = S of s Option.t Ref.t Sq.t
         val x as S (l, r) = S (ref NONE, ref NONE)
         val () = (l := SOME x ; r := SOME x)
      in
         tst (SOME 50)
             (Tie.fix Y
                 (fn s =>
                     iso (data (C1' "S" (sq (refc (option s)))))
                         (fn S ? => ?, S)))
             "S\n\
             \ (#0 as ref\n\
             \   (SOME (S (#0, #1 as ref (SOME (S (#0, #1)))))),\n\
             \  #0 as ref\n\
             \   (SOME (S (#1 as ref (SOME (S (#1, #0))), #0))))"
             x
      end

      $
end
