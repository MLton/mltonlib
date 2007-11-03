(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerDepCases} functor.
 *)
signature LAYER_DEP_CASES_DOM = sig
   include CASES LAYERED_REP
   sharing Open.Rep = Outer
   val iso : ('b, 'y) t -> ('a, 'b) Iso.t -> 'a This.t
   val isoProduct : ('b, 'k, 'y) p -> ('a, 'b) Iso.t -> ('a, 'k) This.p
   val isoSum : ('b, 'y) s -> ('a, 'b) Iso.t -> 'a This.s
   val *` : ('a, 'k, 'x) p * ('b, 'k, 'y) p -> (('a, 'b) Product.t, 'k) This.p
   val T : ('a, 'x) t -> ('a, Generics.Tuple.t) This.p
   val R : Generics.Label.t -> ('a, 'x) t -> ('a, Generics.Record.t) This.p
   val tuple : ('a, Generics.Tuple.t, 'x) p -> 'a This.t
   val record : ('a, Generics.Record.t, 'x) p -> 'a This.t
   val +` : ('a, 'x) s * ('b, 'y) s -> ('a, 'b) Sum.t This.s
   val C0 : Generics.Con.t -> Unit.t This.s
   val C1 : Generics.Con.t -> ('a, 'x) t -> 'a This.s
   val data : ('a, 'x) s -> 'a This.t
   val unit : Unit.t This.t
   val Y : 'a This.t Tie.t
   val --> : ('a, 'x) t * ('b, 'y) t -> ('a -> 'b) This.t
   val exn : Exn.t This.t
   val regExn0 : Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t
   val regExn1 : Generics.Con.t -> ('a, 'x) t -> ('a, Exn.t) Emb.t Effect.t
   val array : ('a, 'x) t -> 'a Array.t This.t
   val refc : ('a, 'x) t -> 'a Ref.t This.t
   val vector : ('a, 'x) t -> 'a Vector.t This.t
   val fixedInt : FixedInt.t This.t
   val largeInt : LargeInt.t This.t
   val largeReal : LargeReal.t This.t
   val largeWord : LargeWord.t This.t
   val word8 : Word8.t  This.t
   val word32 : Word32.t This.t
(*
   val word64 : Word64.t This.t
*)
   val list : ('a, 'x) t -> 'a List.t This.t
   val bool : Bool.t This.t
   val char : Char.t This.t
   val int : Int.t This.t
   val real : Real.t This.t
   val string : String.t This.t
   val word : Word.t This.t
   val hole : 'a This.t Thunk.t
end
