(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic UnitTest

   fun testSR t formats =
       testAll t (fn x =>
          app (fn format => let
                     val fmt = Prettier.render (SOME 5) o fmt t format
                     val expect = fmt x
                  in
                     thatEq string {expect = expect,
                                    actual = fmt (read t expect)}
                   ; thatEq string {expect = expect,
                                    actual = fmt (read t ("( ("^expect^" )) "))}
                  end)
              formats)

   fun testRs t ss =
       test (fn () =>
          app (fn (s, v) =>
                  (thatEq t {expect = v, actual = read t s}
                 ; thatEq t {expect = v, actual = read t (" (( "^s^" ) )")}))
              ss)

   fun fmts f = map (fn v => let open Fmt in default & f := v end)

   local
      open StringCvt
   in
      val radices = [HEX, OCT, BIN, DEC]
      val realFmts = [EXACT, SCI NONE, FIX NONE, GEN NONE]
   end

   val foobar =
       iso (record (R' "foo" int *` R' "+" real *` R' "bar" char))
           (fn {foo = a, + = b, bar = c} => a & b & c,
            fn a & b & c => {foo = a, + = b, bar = c})
in
   val () =
       unitTests
          (title "Generic.Read")

          (testSR (vector (tuple2 (option char, list string))) [Fmt.default])
          (testSR word (fmts Fmt.wordRadix radices))
          (testSR int (fmts Fmt.intRadix radices))
          (testSR real (fmts Fmt.realFmt realFmts))

          (testSR foobar [Fmt.default])

          (testRs foobar [("{+ = 2, bar = #\"3\", foo = 1}",
                           {foo = 1, + = 2.0, bar = #"3"})])

          (testRs unit [("()", ()), ("( )", ())])
          (testRs bool [("true", true), ("false", false)])

          $
end
