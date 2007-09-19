(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Prettier Generic UnitTest

   infix |`

   fun tst n t s v =
       testEq string (fn () => {expect = s, actual = render n (pretty t v)})

   structure Graph = MkGraph (Generic)
   structure BinTree = MkBinTree (Generic)
in
   val () =
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
               "INL LESS\n\
               \& EQUAL\n\
               \& INR GREATER"
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
                 \ (#0=ref\n\
                 \   (SOME (S (#0, #1=ref (SOME (S (#0, #1)))))),\n\
                 \  #1)"
                 x
          end

          (tst (SOME 50)
               (Graph.t int)
               "ref\n\
               \ [VTX\n\
               \   (1,\n\
               \    #0=ref\n\
               \     [VTX\n\
               \       (2,\n\
               \        #4=ref\n\
               \         [VTX\n\
               \           (3,\n\
               \            #5=ref\n\
               \             [VTX (1, #0),\n\
               \              VTX\n\
               \               (6,\n\
               \                #1=ref\n\
               \                 [VTX\n\
               \                   (5,\n\
               \                    #2=ref\n\
               \                     [VTX\n\
               \                       (4,\n\
               \                        #3=ref\n\
               \                         [VTX (6, #1)])])])]),\n\
               \          VTX (5, #2)]),\n\
               \      VTX (4, #3)]),\n\
               \  VTX (2, #4),\n\
               \  VTX (3, #5),\n\
               \  VTX (4, #3),\n\
               \  VTX (5, #2),\n\
               \  VTX (6, #1)]"
               Graph.intGraph1)

          let
             open BinTree Prettier Pretty Pretty.Fixity
             fun withAngles xP x =
                 xP x >>= (fn (_, d) =>
                 return (ATOMIC, angles d))
          in
             tst (SOME 30)
                 (BinTree.t (mapPrinter withAngles int))
                 "BR\n\
                 \ (BR (LF, <0>, LF),\n\
                 \  <1>,\n\
                 \  BR\n\
                 \   (LF,\n\
                 \    <2>,\n\
                 \    BR (LF, <3>, LF)))"
                 (BR (BR (LF, 0, LF), 1, BR (LF, 2, BR (LF, 3, LF))))
          end

          $
end
