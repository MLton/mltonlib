(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerGeneric} functor.
 *)
signature LAYER_GENERIC_DOM = sig
   structure Outer : OPEN_GENERIC
   structure Rep : CLOSED_GENERIC_REP
   structure Result : OPEN_GENERIC_REP
      where type ('a, 'x) t = ('a, 'a Rep.t * 'x) Outer.Rep.t
      where type ('a, 'x) s = ('a, 'a Rep.s * 'x) Outer.Rep.s
      where type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k) Rep.p * 'x) Outer.Rep.p
   val iso : ('b, 'y) Result.t -> ('a, 'b) Iso.t -> 'a Rep.t
   val isoProduct : ('b, 'k, 'y) Result.p -> ('a, 'b) Iso.t -> ('a, 'k) Rep.p
   val isoSum : ('b, 'y) Result.s -> ('a, 'b) Iso.t -> 'a Rep.s
   val *` : ('a, 'k, 'x) Result.p * ('b, 'k, 'y) Result.p -> (('a, 'b) Product.t, 'k) Rep.p
   val T : ('a, 'x) Result.t -> ('a, Generics.Tuple.t) Rep.p
   val R : Generics.Label.t -> ('a, 'x) Result.t -> ('a, Generics.Record.t) Rep.p
   val tuple : ('a, Generics.Tuple.t, 'x) Result.p -> 'a Rep.t
   val record : ('a, Generics.Record.t, 'x) Result.p -> 'a Rep.t
   val +` : ('a, 'x) Result.s * ('b, 'y) Result.s -> (('a, 'b) Sum.t) Rep.s
   val C0 : Generics.Con.t -> Unit.t Rep.s
   val C1 : Generics.Con.t -> ('a, 'x) Result.t -> 'a Rep.s
   val data : ('a, 'x) Result.s -> 'a Rep.t
   val unit : Unit.t Rep.t
   val Y : 'a Rep.t Tie.t
   val --> : ('a, 'x) Result.t * ('b, 'y) Result.t -> ('a -> 'b) Rep.t
   val exn : Exn.t Rep.t
   val regExn : ('a, 'x) Result.s -> ('a, Exn.t) Emb.t Effect.t
   val array : ('a, 'x) Result.t -> 'a Array.t Rep.t
   val refc : ('a, 'x) Result.t -> 'a Ref.t Rep.t
   val vector : ('a, 'x) Result.t -> 'a Vector.t Rep.t
   val largeInt : LargeInt.t Rep.t
   val largeReal : LargeReal.t Rep.t
   val largeWord : LargeWord.t Rep.t
   val word8 : Word8.t  Rep.t
(* val word16 : Word16.t Rep.t (* Word16 not provided by SML/NJ *) *)
   val word32 : Word32.t Rep.t
   val word64 : Word64.t Rep.t
   val list : ('a, 'x) Result.t -> 'a List.t Rep.t
   val bool : Bool.t Rep.t
   val char : Char.t Rep.t
   val int : Int.t Rep.t
   val real : Real.t Rep.t
   val string : String.t Rep.t
   val word : Word.t Rep.t
end
