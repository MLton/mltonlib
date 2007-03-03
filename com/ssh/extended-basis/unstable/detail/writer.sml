(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Writer :> WRITER = struct
   open Writer

   type 'a func_dom = 'a * Univ.t and 'a func_cod = Univ.t
   type 'a func = 'a func_dom -> 'a func_cod
   fun map b2a wA = wA o Pair.map (b2a, Fn.id)

   fun polymorphically uA2uB = let
      val (to, from) = Univ.newIso ()
      fun map f = Pair.map (Fn.id, f)
   in
      Fn.map (map to, from) o uA2uB o Fn.map (map from, to)
   end
end
