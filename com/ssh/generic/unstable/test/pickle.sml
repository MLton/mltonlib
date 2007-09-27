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

          $
end
