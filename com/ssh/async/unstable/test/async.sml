(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Ad hoc tests against the Async module.
 *)
val () = let
   open UnitTest Async Async.Handler
   fun eq (ac, ex) = verifyEq Type.int {actual = ac, expect = ex}
   fun eql (ac, ex) = verifyEq (Type.list Type.int) {actual = ac, expect = ex}
   val full = verifyFailsWith (fn Full => true | _ => false)
   fun inc v _ = v += 1
   fun push l v = List.push (l, v)
in
   unitTests
      (title "Async.IVar")

      (test (fn () => let
                   open IVar
                   val v = new ()
                   val n = ref 0
                in
                   fill v ()
                 ; full (fill v)
                 ; when (read v, inc n) ; eq (!n, 0)
                 ; runAll () ; eq (!n, 1)
                 ; full (fill v)
                 ; when (read v, inc n) ; eq (!n, 1)
                 ; runAll () ; eq (!n, 2)
                 ; runAll () ; eq (!n, 2)
                end))

      (title "Async.MVar")

      (test (fn () => let
                   open MVar
                   val v = new ()
                   val n = ref 0
                in
                   fill v ()
                 ; full (fill v)
                 ; when (take v, inc n) ; eq (!n, 0)
                 ; runAll () ; eq (!n, 1)
                 ; fill v ()
                 ; full (fill v)
                 ; when (take v, inc n) ; eq (!n, 1)
                 ; runAll () ; eq (!n, 2)
                 ; runAll () ; eq (!n, 2)
                end))

      (title "Async.Event.choose")

      (test (fn () => let
                   open Mailbox
                   val b1 = new ()
                   val b2 = new ()
                   val n = ref 0
                   val e = choose [on (take b1, inc n),
                                   on (take b2, inc n)]
                in
                   send b1 ()
                 ; send b1 ()
                 ; send b2 ()
                 ; once e ; eq (!n, 0)
                 ; runAll () ; eq (!n, 1)
                 ; each e ; eq (!n, 1)
                 ; runAll () ; eq (!n, 3)
                 ; runAll () ; eq (!n, 3)
                end))

      (title "Async.Multicast")

      (test (fn () => let
                   open Multicast
                   val c = new ()
                   val () = send c 1
                   val t1 = taker c
                   val () = send c 2
                   val t2 = taker c
                   val () = send c 3
                   val t3 = taker c
                   val () = send c 4
                   val s1 = ref []
                   val s2 = ref []
                   val s3 = ref []
                in
                   all [on (t1, push s1),
                        on (t2, push s2),
                        on (t3, push s3)]
                 ; runAll ()
                 ; eql (!s1, [4, 3, 2])
                 ; eql (!s2, [4, 3])
                 ; eql (!s3, [4])
                end))

      $
end
