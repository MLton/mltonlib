(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Reader :> READER = struct
   open Reader

   fun mapState (s2t, t2s) = Fn.map (s2t, Option.map (Pair.map (Fn.id, t2s)))

   infix >>= <|>

   type 'a etaexp_dom = Univ.t and 'a etaexp_cod = ('a * Univ.t) Option.t
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod

   structure Monad =
      MkMonadP
         (type 'a monad = 'a etaexp
          fun return a s = SOME (a, s)
          fun aM >>= a2bM = Option.mapPartial (Fn.uncurry a2bM) o aM
          fun zero _ = NONE
          fun (lM <|> rM) s = case lM s of NONE => rM s | result => result)

   open Monad

   fun polymorphically aM2bM = let
      val (to, from) = Univ.Iso.new ()
      fun map f = Option.map (Pair.map (Fn.id, f))
   in
      Fn.map (to, map from) o aM2bM o Fn.map (from, map to)
   end
end
