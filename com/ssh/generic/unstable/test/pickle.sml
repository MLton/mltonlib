(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   fun thatSeq t args =
       if seq t (#actual args, #expect args) then () else thatEq t args

   fun thatPU t x =
       case pickle t x
        of p => thatSeq t {expect = x, actual = unpickle t p}

   fun testAllSeq t =
       testAll t (thatPU t)

   fun testSeq t x =
       test (fn () => thatPU t x)

   fun testTypeMismatch t u =
       test (fn () => let
                   val t = Pickle.withTypeHash t
                   val u = Pickle.withTypeHash u
                   val p = pickle t (some t)
                in
                   thatRaises'
                    (fn Pickle.TypeMismatch => ())
                    (fn () => unpickle u p)
                end)
in
   unitTests
    (title "Generic.Pickle")

    (testAllSeq (vector (option (list real))))
    (testAllSeq (tuple2 (fixedInt, largeInt)))
    (testAllSeq (largeReal &` largeWord))
    (testAllSeq (tuple3 (word8, word32, int32)))
    (testAllSeq (bool &` char &` int &` real &` string &` word))

    (title "Generic.Pickle.Cyclic")

    (testSeq (Graph.t int) Graph.intGraph1)
    (testSeq (array exn) ExnArray.exnArray1)

    (title "Generic.Pickle.TypeMismatch")

    (testTypeMismatch int word)
    (testTypeMismatch (list char) (vector word8))
    (testTypeMismatch (array real) (option largeReal))

    (title "Generic.Pickle.Customization")

    (test (fn () => let
        (* This test shows how pickles can be versioned and multiple
         * versions supported at the same time. *)

        open Pickle

        (* First a plain old type rep for our data: *)
        val t1 = record' (R' "id" int
                       *` R' "name" string)
                         (fn {id = a, name = b} => a & b,
                          fn a & b => {id = a, name = b})

        (* Then we assign version {1} to the type: *)
        val t = versioned $ 1 t1

        val v1pickle = pickle t {id = 1, name = "whatever"}

        (* Then a plain old type rep for our new data: *)
        val t2 = record' (R' "id" int
                       *` R' "extra" bool
                       *` R' "name" string)
                         (fn {id = a, extra = b, name = c} => a & b & c,
                          fn a & b & c => {id = a, extra = b, name = c})

        (* Then we assign version {2} to the new type, keeping the version
         * {1} for the old type: *)
        val t = versioned (version 1 t1
                                   (fn {id, name} =>
                                       {id = id, extra = false, name = name}))
                          $ 2 t2

        (* Note that the original versioned {t} is no longer needed.  In
         * an actual program, you would have just edited the original
         * definition instead of introducing a new one.  However, the old
         * type rep is required if you wish to be able to unpickle old
         * versions. *)
     in
        thatEq t {expect = {id = 1, extra = false, name = "whatever"},
                  actual = unpickle t v1pickle}
      ; thatEq t {expect = {id = 3, extra = true, name = "whenever"},
                  actual = unpickle t (pickle t {id = 3, extra = true,
                                                 name = "whenever"})}
     end))

    (title "Generic.Pickle.Format")

    (test (fn () => let
        (* The main purpose of this highly ad hoc test is to help notice
         * when the pickle format changes. *)
        datatype t =
           NIL
         | CON of {bool : Bool.t Vector.t,
                   char : Char.t Ref.t,
                   ints : Int.t * FixedInt.t * LargeInt.t,
                   reals : Real.t * LargeReal.t,
                   string : String.t,
                   words : Word.t * Word8.t * Word32.t * LargeWord.t,
                   unit : Unit.t Option.t Array.t,
                   exn : Exn.t,
                   rest : t} List.t
        val t : t Rep.t = Tie.fix Y (fn t =>
            data
            (isoSum
             (C0'"NIL"
           +` C1'"CON"
                 (list
                  (record
                   (isoProduct
                    (R'"bool" (vector bool)
                  *` R'"char" (refc char)
                  *` R'"ints" (tuple3 (int, fixedInt, largeInt))
                  *` R'"reals" (tuple2 (real, largeReal))
                  *` R'"string" string
                  *` R'"words" (tuple4 (word, word8, word32, largeWord))
                  *` R'"unit" (array (option unit))
                  *` R'"exn" exn
                  *` R'"rest" t)
                    (fn {bool=a, char=b, ints=c, reals=d, string=e, words=f,
                         unit=g, exn=h, rest=i} =>
                        a & b & c & d & e & f & g & h & i,
                     fn a & b & c & d & e & f & g & h & i =>
                        {bool=a, char=b, ints=c, reals=d, string=e, words=f,
                         unit=g, exn=h, rest=i})))))
             (fn NIL => INL () | CON ? => INR ?,
              fn INL () => NIL | INR ? => CON ?)))
        val t = Pickle.withTypeHash t
        val x =
            CON [{bool = Vector.fromList [true, false],
                  char = ref #"z",
                  ints = (1110101, ~102234, 303345223),
                  reals = (1.1111, ~2.2222),
                  string = "string",
                  words = (0wx1FBC2, 0wx2E, 0wxDEADBEEF, 0wx51255D4C),
                  unit = Array.fromList [NONE, SOME ()],
                  exn = Fail "message",
                  rest = NIL}]
     in
        thatEq string
               {expect = "\^A<\249=\^A\^@\^A\^@\^B\^A\^@\^@z\^@\^C\
                         \U\240\^P\^C\166p\254\^DG\174\^T\^R\^@@\
                         \\158^)\203\^P\199\241?@\158^)\203\^P\199\
                         \\^A\192\^@\^Fstring\^@\^C\194\251\^A.\
                         \\239\190\173\222\^DL]%Q\^@\^B\^@\^A\^@\
                         \\^DFail\^@\amessage\^@",
                actual = Byte.bytesToString (pickle t x)}
     end))

    $
end
