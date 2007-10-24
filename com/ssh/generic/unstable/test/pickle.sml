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

   fun thatPU t x = let
      val p = pickle t x
   in
      thatSeq t {expect = x, actual = unpickle t p}
   end

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
          (testAllSeq (tuple3 (word8, word32, word64)))
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
                            open Pickle.P
                         in
                            fn v =>
                               #pickler puInt 1 >>= (fn () => #pickler pu1 v)
                         end,
                         unpickler = let
                            open Pickle.U
                         in
                            #unpickler puInt
                             >>= (fn 1 => #unpickler pu1
                                   | n => raise Fail ("Bad "^Int.toString n))
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
                            open Pickle.P
                         in
                            fn v =>
                               #pickler puInt 2 >>= (fn () => #pickler pu2 v)
                         end,
                         unpickler = let
                            open Pickle.U
                            fun fromR1 {id, name} =
                                {id = id, extra = false, name = name}
                         in
                            #unpickler puInt
                             >>= (fn 1 => #unpickler pu1 >>= return o fromR1
                                   | 2 => #unpickler pu2
                                   | n => raise Fail ("Bad "^Int.toString n))
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

          $
end
