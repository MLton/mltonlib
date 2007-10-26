(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Base signature for a module of directly usable generics.
 *)
signature GENERIC = sig
   include CASES
   structure Rep : CLOSED_REP
      where type  'a      t = ('a,     Unit.t) Open.Rep.t
      where type  'a      s = ('a,     Unit.t) Open.Rep.s
      where type ('a, 'k) p = ('a, 'k, Unit.t) Open.Rep.p
   val iso : ('b, 'y) Open.Rep.t -> ('a, 'b) Iso.t -> 'a Rep.t
   val isoProduct : ('b, 'k, 'y) Open.Rep.p -> ('a, 'b) Iso.t -> ('a, 'k) Rep.p
   val isoSum : ('b, 'y) Open.Rep.s -> ('a, 'b) Iso.t -> 'a Rep.s
   val *` : ('a, 'k, 'x) Open.Rep.p * ('b, 'k, 'y) Open.Rep.p -> (('a, 'b) Product.t, 'k) Rep.p
   val T : ('a, 'x) Open.Rep.t -> ('a, Generics.Tuple.t) Rep.p
   val R : Generics.Label.t -> ('a, 'x) Open.Rep.t -> ('a, Generics.Record.t) Rep.p
   val tuple : ('a, Generics.Tuple.t, 'x) Open.Rep.p -> 'a Rep.t
   val record : ('a, Generics.Record.t, 'x) Open.Rep.p -> 'a Rep.t
   val +` : ('a, 'x) Open.Rep.s * ('b, 'y) Open.Rep.s -> ('a, 'b) Sum.t Rep.s
   val C0 : Generics.Con.t -> Unit.t Rep.s
   val C1 : Generics.Con.t -> ('a, 'x) Open.Rep.t -> 'a Rep.s
   val data : ('a, 'x) Open.Rep.s -> 'a Rep.t
   val unit : Unit.t Rep.t
   val Y : 'a Rep.t Tie.t
   val --> : ('a, 'x) Open.Rep.t * ('b, 'y) Open.Rep.t -> ('a -> 'b) Rep.t
   val exn : Exn.t Rep.t
   val regExn0 : Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t
   val regExn1 : Generics.Con.t -> ('a, 'x) Open.Rep.t -> ('a, Exn.t) Emb.t Effect.t
   val array : ('a, 'x) Open.Rep.t -> 'a Array.t Rep.t
   val refc : ('a, 'x) Open.Rep.t -> 'a Ref.t Rep.t
   val vector : ('a, 'x) Open.Rep.t -> 'a Vector.t Rep.t
   val fixedInt : FixedInt.t Rep.t
   val largeInt : LargeInt.t Rep.t
   val largeReal : LargeReal.t Rep.t
   val largeWord : LargeWord.t Rep.t
   val word8 : Word8.t  Rep.t
   val word32 : Word32.t Rep.t
(*
   val word64 : Word64.t Rep.t
*)
   val list : ('a, 'x) Open.Rep.t -> 'a List.t Rep.t
   val bool : Bool.t Rep.t
   val char : Char.t Rep.t
   val int : Int.t Rep.t
   val real : Real.t Rep.t
   val string : String.t Rep.t
   val word : Word.t Rep.t
end
