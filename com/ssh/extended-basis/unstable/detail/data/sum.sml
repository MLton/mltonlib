(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Sum : SUM = struct
   open Sum

   exception Sum

   fun sum (fA, fB) = fn INL a => fA a | INR b => fB b

   val swap = fn INL x => INR x | INR x => INL x

   val out = fn INL x => x | INR x => x
   val app = sum
   fun map (fA, fB) = sum (INL o fA, INR o fB)

   fun appL f = app (f, ignore)
   fun getL (INL x) _ = x | getL (INR _) x = x
   fun isL (INL _) = true | isL (INR _) = false
   fun mapL f = map (f, Fn.id)
   fun outL (INL l) = l | outL (INR _) = raise Sum

   fun appR f = appL f o swap
   fun getR ? = (getL o swap) ?
   fun isR ? = (isL o swap) ?
   fun mapR f = swap o mapL f o swap
   fun outR ? = (outL o swap) ?

   fun mapLR f = map (f, f)

   fun equal (eqA, eqB) =
       fn (INL l, INL r) => eqA (l, r)
        | (INL _, INR _) => false
        | (INR _, INL _) => false
        | (INR l, INR r) => eqB (l, r)

   fun collate (cmpA, cmpB) =
       fn (INL l, INL r) => cmpA (l, r)
        | (INL _, INR _) => LESS
        | (INR _, INL _) => GREATER
        | (INR l, INR r) => cmpB (l, r)

   fun iso isos = Pair.map (map, map) (Pair.swizzle isos)
end
