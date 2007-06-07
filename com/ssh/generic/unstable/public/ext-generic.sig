(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature EXT_GENERIC = sig
   structure Index : EXT_GENERIC_INDEX
   val iso : ('b, 'y) Index.t -> ('a, 'b) Iso.t -> ('y -> 'x) -> ('a, 'x) Index.t
   val isoProduct : ('b, 'k, 'y) Index.p -> ('a, 'b) Iso.t -> ('y -> 'x) -> ('a, 'k, 'x) Index.p
   val isoSum : ('b, 'y) Index.s -> ('a, 'b) Iso.t -> ('y -> 'x) -> ('a, 'x) Index.s
   val *` : ('a, 'k, 'x) Index.p * ('b, 'k, 'y) Index.p -> ('x * 'y -> 'z)
            -> (('a, 'b) Product.t, 'k, 'z) Index.p
   val T : ('a, 'x) Index.t -> ('x -> 'y) -> ('a, Generics.Tuple.t, 'y) Index.p
   val R : Generics.Label.t -> ('a, 'x) Index.t -> ('x -> 'y) -> ('a, Generics.Record.t, 'y) Index.p
   val tuple : ('a, Generics.Tuple.t, 'x) Index.p -> ('x -> 'y) -> ('a, 'y) Index.t
   val record : ('a, Generics.Record.t, 'x) Index.p -> ('x -> 'y) -> ('a, 'y) Index.t
   val +` : ('a, 'x) Index.s * ('b, 'y) Index.s -> ('x * 'y -> 'z) -> (('a, 'b) Sum.t, 'z) Index.s
   val C0 : Generics.Con.t -> 'x -> (Unit.t, 'x) Index.s
   val C1 : Generics.Con.t -> ('a, 'x) Index.t -> ('x -> 'y) -> ('a, 'y) Index.s
   val data : ('a, 'x) Index.s -> ('x -> 'y) -> ('a, 'y) Index.t
   val unit : 'x -> (Unit.t, 'x) Index.t
   val Y : 'x Tie.t -> ('a, 'x) Index.t Tie.t
   val --> : ('a, 'x) Index.t * ('b, 'y) Index.t -> ('x * 'y -> 'z) -> ('a -> 'b, 'z) Index.t
   val exn : 'x -> (Exn.t, 'x) Index.t
   val regExn : ('a, 'x) Index.s -> ('a, Exn.t) Emb.t -> 'x Effect.t Effect.t
   val array : ('a, 'x) Index.t -> ('x -> 'y) -> ('a Array.t, 'y) Index.t
   val refc : ('a, 'x) Index.t -> ('x -> 'y) -> ('a Ref.t, 'y) Index.t
   val vector : ('a, 'x) Index.t -> ('x -> 'y) -> ('a Vector.t, 'y) Index.t
   val largeInt : 'x -> (LargeInt.t, 'x) Index.t
   val largeReal : 'x -> (LargeReal.t, 'x) Index.t
   val largeWord : 'x -> (LargeWord.t, 'x) Index.t
   val word8 : 'x -> (Word8.t, 'x) Index.t
(* val word16 : 'x -> (Word16.t, 'x) Index.t (* Word16 not provided by SML/NJ *) *)
   val word32 : 'x -> (Word32.t, 'x) Index.t
   val word64 : 'x -> (Word64.t, 'x) Index.t
   val list : ('a, 'x) Index.t -> ('x -> 'y) -> ('a List.t, 'y) Index.t
   val bool : 'x -> (Bool.t, 'x) Index.t
   val char : 'x -> (Char.t, 'x) Index.t
   val int : 'x -> (Int.t, 'x) Index.t
   val real : 'x -> (Real.t, 'x) Index.t
   val string : 'x -> (String.t, 'x) Index.t
   val word : 'x -> (Word.t, 'x) Index.t
end
