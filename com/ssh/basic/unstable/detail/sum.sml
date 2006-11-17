(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Sum : SUM = struct
   datatype ('a, 'b) sum = INL of 'a | INR of 'b
   type('a, 'b) t = ('a, 'b) sum

   exception Sum

   fun sum (fA, fB) = fn INL a => fA a | INR b => fB b

   val swap = fn INL x => INR x | INR x => INL x

   val out = fn INL x => x | INR x => x

   val app = sum
   fun map (fA, fB) = sum (INL o fA, INR o fB)

   val isL = fn INL _ => true | INR _ => false
   val outL = fn INL l => l | INR _ => raise Sum
   val getL = fn INL x => (fn _ => x) | INR _ => (fn x => x)
   fun mapL f = map (f, fn r => r)

   fun isR ? = (isL o swap) ?
   fun outR ? = (outL o swap) ?
   fun getR ? = (getL o swap) ?
   fun mapR f = swap o mapL f o swap

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
end
