(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Reader :> READER = struct
   infix >>=

   structure Monad =
      MkMonad'
         (type ('a, 's) monad = ('a, 's) Reader.t
          fun return a s = SOME (a, s)
          fun rA >>= a2rB = Option.mapPartial (Fn.uncurry a2rB) o rA)

   open Reader Monad

   type univ = Univ.t
   type 'a u = ('a, univ) t

   fun polymorphically uA2uB = let
      val (to, from) = Univ.newIso ()
      fun map f = Option.map (Pair.map (Fn.id, f))
   in
      Fn.map (to, map from) o uA2uB o Fn.map (from, map to)
   end
end
