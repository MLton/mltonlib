(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Reader :> READER = struct
   open Reader

   infix >>= >>&

   fun return a s = SOME (a, s)
   fun rA >>= a2rB = Option.mapPartial (Fn.uncurry a2rB) o rA

   fun map a2b rA = rA >>= return o a2b
   fun rA >>& rB = rA >>= (fn a => rB >>= (fn b => return (Product.& (a, b))))

   type univ = Univ.t
   type 'a u = ('a, univ) t

   fun polymorphically uA2uB = let
      val (to, from) = Univ.newIso ()
      fun map f = Option.map (Pair.map (Fn.id, f))
   in
      Fn.map (to, map from) o uA2uB o Fn.map (from, map to)
   end
end
