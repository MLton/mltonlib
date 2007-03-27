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
   fun eq ex ac = verifyEq Type.int {actual = ac, expect = ex}
   fun eql ex ac = verifyEq (Type.list Type.int) {actual = ac, expect = ex}
   val full = verifyFailsWith (fn Full => true | _ => false)
   fun inc v () = v += 1
   val push = List.push
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
                 ; when (read v) (inc n) ; eq 0 (!n)
                 ; runAll () ; eq 1 (!n)
                 ; full (fill v)
                 ; when (read v) (inc n) ; eq 1 (!n)
                 ; runAll () ; eq 2 (!n)
                 ; runAll () ; eq 2 (!n)
                end))

      (title "Async.MVar")

      (test (fn () => let
                   open MVar
                   val v = new ()
                   val n = ref 0
                in
                   fill v ()
                 ; full (fill v)
                 ; when (take v) (inc n) ; eq 0 (!n)
                 ; runAll () ; eq 1 (!n)
                 ; fill v ()
                 ; full (fill v)
                 ; when (take v) (inc n) ; eq 1 (!n)
                 ; runAll () ; eq 2 (!n)
                 ; runAll () ; eq 2 (!n)
                end))

      (title "Async.choose")

      (test (fn () => let
                   open Mailbox
                   val b1 = new ()
                   val b2 = new ()
                   val n = ref 0
                   val e = choose [on (take b1) (inc n),
                                   on (take b2) (inc n)]
                in
                   send b1 ()
                 ; send b1 ()
                 ; send b2 ()
                 ; once e ; eq 0 (!n)
                 ; runAll () ; eq 1 (!n)
                 ; each e ; eq 1 (!n)
                 ; runAll () ; eq 3 (!n)
                 ; runAll () ; eq 3 (!n)
                end))

      (title "Async.Mailbox")

      (test (fn () => let
                   open Mailbox
                   val b = new ()
                   val s = ref []
                in
                   send b 1
                 ; send b 2
                 ; when (take b) (push s) ; runAll ()
                 ; when (take b) (push s)
                 ; when (take b) (push s) ; runAll ()
                 ; send b 3
                 ; send b 4
                 ; send b 5
                 ; every (take b) (push s) ; runAll ()
                 ; eql [5,4,3,2,1] (!s)
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
                   all [on t1 (push s1),
                        on t2 (push s2),
                        on t3 (push s3)]
                 ; runAll ()
                 ; eql [4, 3, 2] (!s1)
                 ; eql [4, 3] (!s2)
                 ; eql [4] (!s3)
                end))

      (title "Async.SkipCh")

      (test (fn () => let
                   open SkipCh
                   val c = new ()
                in
                   send c 1
                 ; when (take c) (eq 1) ; runAll ()
                 ; send c 2
                 ; send c 3
                 ; when (take c) (eq 3) ; runAll ()
                end))

      (title "Async")

      (test (fn () => let
                   val v = IVar.new ()
                   val c = SkipCh.new ()
                   val l = ref []
                   fun lp () =
                       any [on (SkipCh.take c) (lp o push l),
                            on (IVar.read v) (push l)]
                in
                   lp ()
                 ; runAll ()
                 ; SkipCh.send c 1 ; runAll ()
                 ; SkipCh.send c 2
                 ; SkipCh.send c 3
                 ; SkipCh.send c 4 ; runAll ()
                 ; IVar.fill v 5 ; runAll ()
                 ; eql [5, 4, 2, 1] (!l)
                end))

      $
end
