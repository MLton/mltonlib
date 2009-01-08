(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 * Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure FRU :> FRU = struct
   datatype ('rec, 'upds) t' = IN of 'rec UnOp.t * 'upds
   type ('rec, 'upds, 'data) t =
        (('rec, 'upds) t', ('rec, 'upds) t', 'data UnOp.t) Fold.t
   type ('value, 'rec) upd = 'value -> 'rec UnOp.t
   type ('args, 'upds, 'result) args =
        (('args, 'upds) t', ('args, 'upds) t', 'result) Fold.t

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

      fun args ? =
          Fold.post
           (fn mkU => fn iso1 => fn iso2 => fn default => fn f =>
               Fold.post
                (fn u => f (u default))
                (updData Iso.id (mkU iso1 iso2)))
           make
           ?

      fun U s v =
          Fold.mapSt (fn IN (f, u) => IN (s u v o f, u))

      fun D r =
          Fold.mapSt (fn IN (_, u) => IN (Fn.const r, u))
   end
end
