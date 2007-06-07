(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature EXT_GENERIC = sig
   structure Index : EXT_GENERIC_INDEX
   val iso : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'y) Index.t -> ('a, 'b) Iso.t -> ('a, 'x) Index.t
   val isoProduct : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'k, 'y) Index.p -> ('a, 'b) Iso.t -> ('a, 'k, 'x) Index.p
   val isoSum : ('y -> ('a, 'b) Iso.t -> 'x) -> ('b, 'y) Index.s -> ('a, 'b) Iso.t -> ('a, 'x) Index.s
   val *` : ('x * 'y -> 'z) -> ('a, 'k, 'x) Index.p * ('b, 'k, 'y) Index.p -> (('a, 'b) Product.t, 'k, 'z) Index.p
   val T : ('x -> 'y) -> ('a, 'x) Index.t -> ('a, Generics.Tuple.t, 'y) Index.p
   val R : (Generics.Label.t -> 'x -> 'y) -> Generics.Label.t -> ('a, 'x) Index.t -> ('a, Generics.Record.t, 'y) Index.p
   val tuple : ('x -> 'y) -> ('a, Generics.Tuple.t, 'x) Index.p -> ('a, 'y) Index.t
   val record : ('x -> 'y) -> ('a, Generics.Record.t, 'x) Index.p -> ('a, 'y) Index.t
   val +` : ('x * 'y -> 'z) -> ('a, 'x) Index.s * ('b, 'y) Index.s -> (('a, 'b) Sum.t, 'z) Index.s
   val C0 : (Generics.Con.t -> 'x) -> Generics.Con.t -> (Unit.t, 'x) Index.s
   val C1 : (Generics.Con.t -> 'x -> 'y) -> Generics.Con.t -> ('a, 'x) Index.t -> ('a, 'y) Index.s
   val data : ('x -> 'y) -> ('a, 'x) Index.s -> ('a, 'y) Index.t
   val unit : 'x -> (Unit.t, 'x) Index.t
   val Y : 'x Tie.t -> ('a, 'x) Index.t Tie.t
   val --> : ('x * 'y -> 'z) -> ('a, 'x) Index.t * ('b, 'y) Index.t -> ('a -> 'b, 'z) Index.t
   val exn : 'x -> (Exn.t, 'x) Index.t
   val regExn : ('x -> ('a, Exn.t) Emb.t Effect.t) -> ('a, 'x) Index.s -> ('a, Exn.t) Emb.t Effect.t
   val array : ('x -> 'y) -> ('a, 'x) Index.t -> ('a Array.t, 'y) Index.t
   val refc : ('x -> 'y) -> ('a, 'x) Index.t -> ('a Ref.t, 'y) Index.t
   val vector : ('x -> 'y) -> ('a, 'x) Index.t -> ('a Vector.t, 'y) Index.t
   val largeInt : 'x -> (LargeInt.t, 'x) Index.t
   val largeReal : 'x -> (LargeReal.t, 'x) Index.t
   val largeWord : 'x -> (LargeWord.t, 'x) Index.t
   val word8 : 'x -> (Word8.t, 'x) Index.t
(* val word16 : 'x -> (Word16.t, 'x) Index.t (* Word16 not provided by SML/NJ *) *)
   val word32 : 'x -> (Word32.t, 'x) Index.t
   val word64 : 'x -> (Word64.t, 'x) Index.t
   val list : ('x -> 'y) -> ('a, 'x) Index.t -> ('a List.t, 'y) Index.t
   val bool : 'x -> (Bool.t, 'x) Index.t
   val char : 'x -> (Char.t, 'x) Index.t
   val int : 'x -> (Int.t, 'x) Index.t
   val real : 'x -> (Real.t, 'x) Index.t
   val string : 'x -> (String.t, 'x) Index.t
   val word : 'x -> (Word.t, 'x) Index.t
end
