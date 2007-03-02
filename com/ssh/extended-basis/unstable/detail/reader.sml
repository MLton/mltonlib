(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Reader :> READER = struct
   open Reader

   infix >>=

   type 'a monad_d = Univ.t and 'a monad_r = ('a * Univ.t) Option.t

   structure Monad =
      MkMonadP
         (type 'a monad = 'a monad_d -> 'a monad_r
          fun return a s = SOME (a, s)
          fun aM >>= a2bM = Option.mapPartial (Fn.uncurry a2bM) o aM
          fun zero _ = NONE
          fun plus (lM, rM) s = case lM s of NONE => rM s | result => result)

   open Monad

   fun polymorphically aM2bM = let
      val (to, from) = Univ.newIso ()
      fun map f = Option.map (Pair.map (Fn.id, f))
   in
      Fn.map (to, map from) o aM2bM o Fn.map (from, map to)
   end
end
