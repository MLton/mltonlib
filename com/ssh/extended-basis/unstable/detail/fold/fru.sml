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
      datatype product = datatype Product.product
      datatype sum = datatype Sum.sum
      infix &

      fun fin (m, u) iso (_, p2r) =
          p2r (m (Fn.map iso o u))

      fun make ? =
          Fold.NSZ.wrap {none = fin, some = fin, zero = (Fn.const (), Fn.id)} ?

      fun out (IN ?) = ?

      fun updData iso u =
          Fold.wrap (IN (Fn.id, u), Fn.map iso o Pair.fst o out)
   in
      fun A ? =
          Fold.NSZ.mapSt
             {none = Pair.map (Fn.const Fn.id, Fn.const Fn.const),
              some = Pair.map (fn m => fn p => m (p o INL) & (p o INR),
                               fn u => fn INL p => (fn l & r => u p l & r)
                                        | INR v => (fn l & _ => l & v))} ?

      fun fruData (iso : ('data, 'rec) Iso.t) =
          Fold.post (fn f => fn ~ => updData iso o f ~) make

      fun fru ? =
          fruData Iso.id ?

      fun U s v =
          Fold.mapSt (fn IN (f, u) => IN (s u v o f, u))
   end
end
