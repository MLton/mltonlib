(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   (* <-- SML/NJ workaround *)
   infix <^>
   (* SML/NJ workaround --> *)

   open Generic UnitTest

   fun thatSeq t args =
       if seq t (#actual args, #expect args) then () else thatEq t args

   fun testSR t formats =
       testAll t (fn x =>
          app (fn format =>
                  thatSeq t {expect = x,
                             actual = read t (Prettier.render
                                               (SOME 5) (fmt t format x))})
              formats)

   fun testRs t ss =
       test (fn () =>
                app (fn (s, v) => thatEq t {expect = v, actual = read t s}) ss)

   fun fmts f = map (fn v => let open Fmt in default & f := v end)

   local
      open StringCvt
   in
      val radices = [HEX, OCT, BIN, DEC]
   end

   local
      open Prettier Pretty
   in
      fun ps t =
          mapPrinter
           (fn p => fn x =>
               p x >>= (fn (a, d) =>
               return (if Word32.isOdd (hash t x)
                       then (a, d)
                       else (Fixity.ATOMIC,
                             txt " (* (*:-)*) *) ( (* :-( *) " <^> d <^>
                             txt " (*) *) ) (* foo *) "))))
             t
   end

   val array = fn ? => ps (array ?)
   val bool = ps bool
   val char = ps char
   val int = ps int
   val list = fn ? => ps (list ?)
   val option = fn ? => ps (option ?)
   val order = ps order
   val string = ps string
   val tuple2 = fn ? => ps (tuple2 ?)
   val unit = ps unit
   val vector = fn ? => ps (vector ?)
   val word = ps word
   val foobar =
       ps (record' (R' "foo" bool *` R' "+" unit *` R' "bar" char)
                   (fn {foo = a, + = b, bar = c} => a & b & c,
                    fn a & b & c => {foo = a, + = b, bar = c}))
in
   unitTests
    (title "Generic.Read")

    (testSR word (fmts Fmt.wordRadix radices))
    (testSR int (fmts Fmt.intRadix radices))

    (testSR (array (refc order)) [Fmt.default])

    (testSR foobar [Fmt.default])

    (testRs foobar [("{+ = ( ( ) ) , bar = #\"3\", foo = true}",
                     {foo = true, + = (), bar = #"3"})])

    (testRs (tuple2 (int, string))
            [("{1 = 3, 2 = \"4\"}",
              {1 = 3, 2 = "4"}),
             ("((*;)*)({2 = \"2\", 1 = 1}(*;)*))) (*;)*)",
              {1 = 1, 2 = "2"}),
             ("(2, \"1\")",
              (2, "1"))])

    (testRs real [("-2.0e~10", ~2.0e~10), (" ( 1.2 ) ", 1.2)])

    (testSR (tuple2 (tuple2 (string, vector (option unit)), list char))
            [Fmt.default])

    (testFails (fn () => read int "0 garbage accepted"))

    $
end
