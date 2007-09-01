(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure Generic = struct
      open Generic
      local
         structure Open = WithSeq (open Generic Open)
         structure Extra = CloseWithExtra (Open)
      in
         open Open Extra
      end
   end

   structure Graph = MkGraph (Generic)
   structure ExnArray = MkExnArray (Generic)

   open Generic UnitTest

   fun chkSeq t =
       (chk o all t)
          (fn x => let
                 val p = pickle t x
              in
                 that (seq t (x, unpickle t p))
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
   val () =
       unitTests
          (title "Generic.Pickle")

          (chkSeq (vector (option (list real))))
          (chkSeq (tuple2 (fixedInt, largeInt)))
          (chkSeq (largeReal &` largeWord))
          (chkSeq (tuple3 (word8, word32, word64)))
          (chkSeq (bool &` char &` int &` real &` string &` word))

          (title "Generic.Pickle.Cyclic")

          (testSeq (Graph.t int) Graph.intGraph1)
          (testSeq (array exn) ExnArray.exnArray1)

          (title "Generic.Pickle.TypeMismatch")

          (testTypeMismatch int word)
          (testTypeMismatch (list char) (vector char))
          (testTypeMismatch (array real) (option real))

          $
end
