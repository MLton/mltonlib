(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Support for functional record update.
 *
 * See
 *
 *   http://mlton.org/FunctionalRecordUpdate
 *
 * for further information.
 *)

structure FRU = struct
   fun make ? = let
      fun fin (m, u) =
          fn iso : ('r1, 'p1) Iso.t =>
             fn (_, p2r') : ('r2, 'p2) Iso.t =>
                p2r' (m (Fn.map iso o u))
   in
      Fold.NSZ.wrap {none = fin, some = fin,
                     zero = (const (), id)}
   end ?

   fun A ? =
       Fold.NSZ.mapSt
          {none = Pair.map (const id, const const),
           some = Pair.map (fn m =>
                               fn p =>
                                  m (p o INL) & (p o INR),
                            fn u =>
                               fn INL p =>
                                  (fn l & r => u p l & r)
                                | INR v =>
                                  (fn l & _ => l & v))} ?

   (* 2^n *)
   val A1 = A
   fun A2 ? = pass ? A1 A1
   fun A4 ? = pass ? A2 A2
   fun A8 ? = pass ? A4 A4

   (* 2^i + j where j < 2^i *)
   fun A3  ? = pass ? A2 A1
   fun A5  ? = pass ? A4 A1
   fun A6  ? = pass ? A4 A2
   fun A7  ? = pass ? A4 A3
   fun A9  ? = pass ? A8 A1
   fun A10 ? = pass ? A8 A2
   fun A11 ? = pass ? A8 A3
   fun A12 ? = pass ? A8 A4
   fun A13 ? = pass ? A8 A5
   fun A14 ? = pass ? A8 A6
   fun A15 ? = pass ? A8 A7

   fun updData iso u = Fold.wrap ((id, u), Fn.map iso o Pair.fst)
   fun fruData iso = Fold.post (fn f => fn ~ => updData iso o f ~) make

   fun upd ? = updData Iso.id ?
   fun fru ? = fruData Iso.id ?

   fun U s v = Fold.mapSt (fn (f, u) => (s u v o f, u))
end

val U = FRU.U
