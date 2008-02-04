(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure Graph = MkGraph (Generic)
   structure ExnArray = MkExnArray (Generic)

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
   val () =
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

              open Cvt Pickle

              val puInt = getPU int

              (* First a plain old type rep for our data: *)
              val t1 = iso (record (R' "id" int
                                 *` R' "name" string))
                           (fn {id = a, name = b} => a & b,
                            fn a & b => {id = a, name = b})

              (* Then we customize it to store and check a version number: *)
              val pu1 = getPU t1
              val t =
                  setPU {pickler = let
                            open P
                         in
                            fn v =>
                               #pickler puInt 1 >>= (fn () => #pickler pu1 v)
                         end,
                         unpickler = let
                            open U
                         in
                            #unpickler puInt
                             >>= (fn 1 => #unpickler pu1
                                   | n => fails ["Bad ", D n])
                         end}
                        t1

              val pickled = pickle t {id = 1, name = "whatever"}

              (* Then a plain old type rep for our new data: *)
              val t2 = iso (record (R' "id" int
                                 *` R' "extra" bool
                                 *` R' "name" string))
                           (fn {id = a, extra = b, name = c} => a & b & c,
                            fn a & b & c => {id = a, extra = b, name = c})

              (* Then we customize it to store a version number and dispatch
               * based on it: *)
              val pu2 = getPU t2
              val t =
                  setPU {pickler = let
                            open P
                         in
                            fn v =>
                               #pickler puInt 2 >>= (fn () => #pickler pu2 v)
                         end,
                         unpickler = let
                            open U
                            fun fromR1 {id, name} =
                                {id = id, extra = false, name = name}
                         in
                            #unpickler puInt
                             >>= (fn 1 => map fromR1 (#unpickler pu1)
                                   | 2 => #unpickler pu2
                                   | n => fails ["Bad ", D n])
                         end}
                        t2
              (* Note that the original customized {t} is no longer
               * needed.  In an actual program, you would have just edited
               * the original definition instead of introducing a new one.
               * However, the old type rep is required if you wish to be
               * able to unpickle old versions. *)
           in
              thatEq t {expect = {id = 1, extra = false, name = "whatever"},
                        actual = unpickle t pickled}
            ; thatEq t {expect = {id = 3, extra = true, name = "whenever"},
                        actual = unpickle t (pickle t {id = 3, extra = true,
                                                       name = "whenever"})}
           end))

          (title "Pickle.Format")

          (test (fn () => let
              (* The main purpose of this highly ad hoc test is to help
               * notice when the pickle format changes. *)
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
              thatEq string {expect = "\^A<\249=\^A\^@\^A\^@\^B\^A\^@\^@z\^@\^C\
                                      \U\240\^P\^C\166p\254\^DG\174\^T\^R\^@@\
                                      \\158^)\203\^P\199\241?@\158^)\203\^P\199\
                                      \\^A\192\^@\^Fstring\^@\^C\194\251\^A.\
                                      \\239\190\173\222\^DL]%Q\^@\^B\^@\^A\^@\
                                      \\^DFail\^@\amessage\^@",
                             actual = pickle t x}
           end))

          $
end
