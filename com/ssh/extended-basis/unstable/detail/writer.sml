(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Writer :> WRITER = struct
   open Writer

   type ('a, 's) func = ('a, 's) t
   fun map b2a wA = wA o Pair.map (b2a, Fn.id)

   type univ = Univ.t
   type 'a u = ('a, univ) t

   fun polymorphically uA2uB = let
      val (to, from) = Univ.newIso ()
      fun map f = Pair.map (Fn.id, f)
   in
      Fn.map (map to, from) o uA2uB o Fn.map (map from, to)
   end
end
