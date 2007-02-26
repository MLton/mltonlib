(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open UnitTest Async Async.Event
   fun eq (ac, ex) = verifyEq Type.int {actual = ac, expect = ex}
   val full = verifyFailsWith (fn Full => true | _ => false)
   fun inc v _ = v += 1
in
   unitTests
      (title "Async.IVar")

      (test (fn () => let
                   val v = IVar.new ()
                   val n = ref 0
                in
                   IVar.fill v ()
                 ; full (IVar.fill v)
                 ; when (IVar.read v, inc n)
                 ; eq (!n, 0)
                 ; Handler.runAll ()
                 ; eq (!n, 1)
                 ; full (IVar.fill v)
                 ; when (IVar.read v, inc n)
                 ; eq (!n, 1)
                 ; Handler.runAll ()
                 ; eq (!n, 2)
                 ; Handler.runAll ()
                 ; eq (!n, 2)
                end))

      (title "Async.Event.choose")

      (test (fn () => let
                   val b1 = Mailbox.new ()
                   val b2 = Mailbox.new ()
                   val n = ref 0
                   val e = choose [on (Mailbox.take b1, inc n),
                                   on (Mailbox.take b2, inc n)]
                in
                   Mailbox.send b1 ()
                 ; Mailbox.send b1 ()
                 ; Mailbox.send b2 ()
                 ; once e ; eq (!n, 0)
                 ; Handler.runAll () ; eq (!n, 1)
                 ; each e ; eq (!n, 1)
                 ; Handler.runAll () ; eq (!n, 3)
                 ; Handler.runAll () ; eq (!n, 3)
                end))

      $
end
