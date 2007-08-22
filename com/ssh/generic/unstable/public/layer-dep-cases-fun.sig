(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerDepCases} functor.
 *)
signature LAYER_DEP_CASES_DOM = sig
   structure Outer : OPEN_CASES
   structure Result : LAYERED_REP
   sharing Outer.Rep = Result.Outer
   val iso : ('b, 'y) Result.t -> ('a, 'b) Iso.t -> 'a Result.Closed.t
   val isoProduct : ('b, 'k, 'y) Result.p -> ('a, 'b) Iso.t -> ('a, 'k) Result.Closed.p
   val isoSum : ('b, 'y) Result.s -> ('a, 'b) Iso.t -> 'a Result.Closed.s
   val *` : ('a, 'k, 'x) Result.p * ('b, 'k, 'y) Result.p -> (('a, 'b) Product.t, 'k) Result.Closed.p
   val T : ('a, 'x) Result.t -> ('a, Generics.Tuple.t) Result.Closed.p
   val R : Generics.Label.t -> ('a, 'x) Result.t -> ('a, Generics.Record.t) Result.Closed.p
   val tuple : ('a, Generics.Tuple.t, 'x) Result.p -> 'a Result.Closed.t
   val record : ('a, Generics.Record.t, 'x) Result.p -> 'a Result.Closed.t
   val +` : ('a, 'x) Result.s * ('b, 'y) Result.s -> (('a, 'b) Sum.t) Result.Closed.s
   val C0 : Generics.Con.t -> Unit.t Result.Closed.s
   val C1 : Generics.Con.t -> ('a, 'x) Result.t -> 'a Result.Closed.s
   val data : ('a, 'x) Result.s -> 'a Result.Closed.t
   val unit : Unit.t Result.Closed.t
   val Y : 'a Result.Closed.t Tie.t
   val --> : ('a, 'x) Result.t * ('b, 'y) Result.t -> ('a -> 'b) Result.Closed.t
   val exn : Exn.t Result.Closed.t
   val regExn0 : Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t
   val regExn1 : Generics.Con.t -> ('a, 'x) Result.t -> ('a, Exn.t) Emb.t Effect.t
   val array : ('a, 'x) Result.t -> 'a Array.t Result.Closed.t
   val refc : ('a, 'x) Result.t -> 'a Ref.t Result.Closed.t
   val vector : ('a, 'x) Result.t -> 'a Vector.t Result.Closed.t
   val fixedInt : FixedInt.t Result.Closed.t
   val largeInt : LargeInt.t Result.Closed.t
   val largeReal : LargeReal.t Result.Closed.t
   val largeWord : LargeWord.t Result.Closed.t
   val word8 : Word8.t  Result.Closed.t
   val word32 : Word32.t Result.Closed.t
   val word64 : Word64.t Result.Closed.t
   val list : ('a, 'x) Result.t -> 'a List.t Result.Closed.t
   val bool : Bool.t Result.Closed.t
   val char : Char.t Result.Closed.t
   val int : Int.t Result.Closed.t
   val real : Real.t Result.Closed.t
   val string : String.t Result.Closed.t
   val word : Word.t Result.Closed.t
end
