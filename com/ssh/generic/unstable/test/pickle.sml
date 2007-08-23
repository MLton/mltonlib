(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Generic UnitTest

   fun chkEq t =
       (chk o all t)
          (fn x => let
                 val p = pickle t x
              in
                 that (eq t (x, unpickle t p))
              end)

   fun testSeq t x =
       test (fn () => let
                   val p = pickle t x
                in
                   verifyTrue (seq t (x, unpickle t p))
                end)

   fun testTypeMismatch t u =
       test (fn () => let
                   val p = pickle t (some t)
                in
                   verifyFailsWith
                      (fn Pickling.TypeMismatch => true | _ => false)
                      (fn () => unpickle u p)
                end)
in
   unitTests
      (title "Generic.Pickle")

      (chkEq (vector (option (list real))))
      (chkEq (tuple2 (fixedInt, largeInt)))
      (chkEq (largeReal &` largeWord))
      (chkEq (tuple3 (word8, word32, word64)))
      (chkEq (bool &` char &` int &` real &` string &` word))

      (title "Generic.Pickle.Cyclic")

      (testSeq (Graph.t int) Graph.intGraph1)
      (testSeq (array exn) ExnArray.exnArray1)

      (title "Generic.Pickle.TypeMismatch")

      (testTypeMismatch int word)
      (testTypeMismatch (list char) (vector char))
      (testTypeMismatch (array real) (option real))

      $
end
