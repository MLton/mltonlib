(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for open structural cases.  This is derived from the
 * {CLOSED_CASES} signature by parameterizing the representation to allow
 * building extended representations.
 *)
signature OPEN_CASES = sig
   structure Rep : OPEN_REP
   val iso : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'y) Rep.t -> ('a, 'b) Iso.t -> ('a, 'x) Rep.t
   val isoProduct : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'k, 'y) Rep.p -> ('a, 'b) Iso.t -> ('a, 'k, 'x) Rep.p
   val isoSum : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'y) Rep.s -> ('a, 'b) Iso.t -> ('a, 'x) Rep.s
   val *` : ('x * 'y -> 'z) -> ('a, 'k, 'x) Rep.p * ('b, 'k, 'y) Rep.p -> (('a, 'b) Product.t, 'k, 'z) Rep.p
   val T : ('x -> 'y) -> ('a, 'x) Rep.t -> ('a, Generics.Tuple.t, 'y) Rep.p
   val R : (Generics.Label.t -> 'x -> 'y) -> Generics.Label.t -> ('a, 'x) Rep.t -> ('a, Generics.Record.t, 'y) Rep.p
   val tuple : ('x -> 'y) -> ('a, Generics.Tuple.t, 'x) Rep.p -> ('a, 'y) Rep.t
   val record : ('x -> 'y) -> ('a, Generics.Record.t, 'x) Rep.p -> ('a, 'y) Rep.t
   val +` : ('x * 'y -> 'z) -> ('a, 'x) Rep.s * ('b, 'y) Rep.s -> (('a, 'b) Sum.t, 'z) Rep.s
   val C0 : (Generics.Con.t -> 'x) -> Generics.Con.t -> (Unit.t, 'x) Rep.s
   val C1 : (Generics.Con.t -> 'x -> 'y) -> Generics.Con.t -> ('a, 'x) Rep.t -> ('a, 'y) Rep.s
   val data : ('x -> 'y) -> ('a, 'x) Rep.s -> ('a, 'y) Rep.t
   val unit : 'x -> (Unit.t, 'x) Rep.t
   val Y : 'x Tie.t -> ('a, 'x) Rep.t Tie.t
   val --> : ('x * 'y -> 'z) -> ('a, 'x) Rep.t * ('b, 'y) Rep.t -> ('a -> 'b, 'z) Rep.t
   val exn : 'x -> (Exn.t, 'x) Rep.t
   val regExn0 : (Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t) -> Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t
   val regExn1 : (Generics.Con.t -> 'x -> ('a, Exn.t) Emb.t Effect.t) -> Generics.Con.t -> ('a, 'x) Rep.t -> ('a, Exn.t) Emb.t Effect.t
   val array : ('x -> 'y) -> ('a, 'x) Rep.t -> ('a Array.t, 'y) Rep.t
   val refc : ('x -> 'y) -> ('a, 'x) Rep.t -> ('a Ref.t, 'y) Rep.t
   val vector : ('x -> 'y) -> ('a, 'x) Rep.t -> ('a Vector.t, 'y) Rep.t
   val fixedInt : 'x -> (FixedInt.t, 'x) Rep.t
   val largeInt : 'x -> (LargeInt.t, 'x) Rep.t
   val largeReal : 'x -> (LargeReal.t, 'x) Rep.t
   val largeWord : 'x -> (LargeWord.t, 'x) Rep.t
   val word8 : 'x -> (Word8.t, 'x) Rep.t
   val word32 : 'x -> (Word32.t, 'x) Rep.t
(*
   val word64 : 'x -> (Word64.t, 'x) Rep.t
*)
   val list : ('x -> 'y) -> ('a, 'x) Rep.t -> ('a List.t, 'y) Rep.t
   val bool : 'x -> (Bool.t, 'x) Rep.t
   val char : 'x -> (Char.t, 'x) Rep.t
   val int : 'x -> (Int.t, 'x) Rep.t
   val real : 'x -> (Real.t, 'x) Rep.t
   val string : 'x -> (String.t, 'x) Rep.t
   val word : 'x -> (Word.t, 'x) Rep.t
   val hole : 'x -> ('a, 'x) Rep.t
end
