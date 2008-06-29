(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Prettier Generic UnitTest

   infix |`

   fun tst n f t s v =
       testEq string (fn () => {expect = s, actual = render n (fmt t f v)})
in
   unitTests
    (title "Generic.Pretty")

    (tst NONE Fmt.default unit "()" ())

    (tst NONE Fmt.default word "0wx15" 0wx15)

    (tst (SOME 6) Fmt.default (list int)
         "[1,\n 2,\n 3]"
         [1, 2, 3])

    (tst (SOME 2) Fmt.default (vector bool)
         "#[true,\n\
         \  false]"
         (Vector.fromList [true, false]))

    (tst (SOME 15) Fmt.default (tuple3 (option unit, string, exn))
         "(NONE,\n\
         \ \"a\",\n\
         \ Empty)"
         (NONE, "a", Empty))

    (tst NONE Fmt.default (array unit) "#()" (Array.array (0, ())))

    (tst NONE Fmt.default real "~3.141" ~3.141)

    (tst (SOME 22) Fmt.default
         ((order |` unit) &` order &` (unit |` order))
         "INL LESS\n\
         \& EQUAL\n\
         \& INR GREATER"
         (INL LESS & EQUAL & INR GREATER))

    let
       fun chk s e = tst (SOME 11) Fmt.default string e s
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
       tst NONE Fmt.default exn "#Unknown" Unknown
    end

    (tst (SOME 9)
         let open Fmt in default & fieldNest := SOME 4 end
         (record' (R' "1" int
                *` R' "+" (unOp int)
                *` R' "long" char)
                  (fn {1 = a, + = b, long = c} => a & b & c,
                   fn a & b & c => {1 = a, + = b, long = c}))
         "{1 = 200000000,\n\
         \ + = #fn,\n\
         \ long =\n\
         \     #\"d\"}"
         {1 = 200000000, + = id, long = #"d"})

    let
       datatype s = S of s Option.t Ref.t Sq.t
       val x as S (l, r) = S (ref NONE, ref NONE)
       val () = (l := SOME x ; r := SOME x)
    in
       tst (SOME 50) Fmt.default
           ((Tie.fix Y)
             (fn s =>
                 data' (C1' "S" (sq (refc (option s))))
                       (fn S ? => ?, S)))
           "S\n\
           \ (#0=ref\n\
           \   (SOME (S (#0, #1=ref (SOME (S (#0, #1)))))),\n\
           \  #1)"
           x
    end

    (tst (SOME 50) Fmt.default (Graph.t int)
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
           xP x >>= (fn (_, d) => return (ATOMIC, angles d))
    in
       tst (SOME 30)
           let open Fmt in default & conNest := NONE end
           (BinTree.t (mapPrinter withAngles int))
           "BR (BR (LF, <0>, LF),\n\
           \    <1>,\n\
           \    BR (LF,\n\
           \        <2>,\n\
           \        BR (LF, <3>, LF)))"
           (BR (BR (LF, 0, LF), 1, BR (LF, 2, BR (LF, 3, LF))))
    end

    (tst NONE let open Fmt in default & intRadix := StringCvt.HEX end
         int "~0x10" ~16)

    $
end
