(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure IOSMonad :> IOS_MONAD = struct
   datatype sum = datatype Sum.sum

   type ('a, 's) t = 's -> 'a * 's

   exception EOS

   fun fromReader rA s = case rA s of NONE => raise EOS | SOME ? => ?
   fun fromWriter wV v s = ((), wV (v, s))
   fun fromPutter pV v s = (pV (s, v), s)

   fun mapState (s2t, t2s) tT = Pair.map (Fn.id, t2s) o tT o s2t

   fun map a2b aT = Pair.map (a2b, Fn.id) o aT

   fun return v s = (v, s)
   fun op >>= (aT, a2bT) = Fn.uncurry a2bT o aT
end
