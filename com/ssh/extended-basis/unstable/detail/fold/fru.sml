(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure FRU :> FRU = struct
   datatype ('rec, 'upds) t' = IN of 'rec UnOp.t * 'upds
   type ('rec, 'upds, 'data) t =
        (('rec, 'upds) t', ('rec, 'upds) t', 'data UnOp.t) Fold.t

   local
      open StaticSum
      datatype product = datatype Product.product
      infix &

      fun fin (m, u) iso (_, p2r) =
          p2r (m (Fn.map iso o u))

      fun make ? =
          Fold.wrap (StaticSum.inL (Fn.const (), Fn.id), fin o out) ?

      fun out (IN ?) = ?

      fun updData iso u =
          Fold.wrap (IN (Fn.id, u), Fn.map iso o Pair.fst o out)
   in
      fun A ? =
          Fold.mapSt
             (inR o sum (Pair.map (Fn.const Fn.id, Fn.const Fn.const),
                         Pair.map (fn m => fn p => m (p o inL) & (p o inR),
                                   fn u => sum (fn p => fn l & r => u p l & r,
                                                fn v => fn l & _ => l & v)))) ?

      fun fruData iso =
          Fold.post (fn f => fn ~ => updData iso o f ~) make

      fun fru ? =
          fruData Iso.id ?

      fun U s v =
          Fold.mapSt (fn IN (f, u) => IN (s u v o f, u))
   end
end
