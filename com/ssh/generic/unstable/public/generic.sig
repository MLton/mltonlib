(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for type-indexed values based on a generic representation
 * of datatypes.
 *)
signature GENERIC = sig
   structure Index : GENERIC_INDEX

   (** == SUPPORT FOR USER-DEFINED TYPES == *)

   val iso : 'b Index.t -> ('a, 'b) Iso.t -> 'a Index.t
   (**
    * Given a type-index {'b Index.t} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a Index.t}.  The purpose of {iso} is to
    * support user-defined types.
    *)

   val isoProduct : ('b, 'k) Index.p -> ('a, 'b) Iso.t -> ('a, 'k) Index.p
   (**
    * Given a type-index {('b, 'k) Index.p} and an isomorphism between
    * {'a} and {'b}, returns a type-index {('a, 'k) Index.p}.
    *)

   val isoSum : 'b Index.s -> ('a, 'b) Iso.t -> 'a Index.s
   (**
    * Given a type-index {'b Index.s} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a Index.s}.
    *)

   (** == SUPPORT FOR TUPLES AND RECORDS == *)

   val *` :
       ('a, 'k) Index.p * ('b, 'k) Index.p -> (('a, 'b) Product.t, 'k) Index.p
   (**
    * Given type-indices for fields of type {'a} and {'b} of the same kind
    * {'k} (tuple or record), returns a type-index for the product {('a,
    * 'b) Product.t}.
    *)

   val T : 'a Index.t -> ('a, Generics.Tuple.t) Index.p
   (** Specifies a field of a tuple. *)

   val R : Generics.Label.t -> 'a Index.t -> ('a, Generics.Record.t) Index.p
   (** Specifies a field of a record. *)

   val tuple  : ('a, Generics.Tuple.t) Index.p -> 'a Index.t
   (** Specifies a tuple. *)

   val record : ('a, Generics.Record.t) Index.p -> 'a Index.t
   (** Specifies a record. *)

   (** == SUPPORT FOR DATATYPES == *)

   val +` : 'a Index.s * 'b Index.s -> (('a, 'b) Sum.t) Index.s
   (**
    * Given type-indices for variants of type {'a} and {'b}, returns a
    * type-index for the sum {('a, 'b) Sum.t}.
    *)

   val C0 : Generics.Con.t -> Unit.t Index.s
   (** Specifies a nullary constructor. *)

   val C1 : Generics.Con.t -> 'a Index.t -> 'a Index.s
   (** Specifies a unary constructor. *)

   val data : 'a Index.s -> 'a Index.t
   (** Specifies a complete datatype. *)

   val unit : Unit.t Index.t
   (**
    * Type-index for the {unit} type.  Using {unit} and {+} one can
    * actually encode {bool}, {word}, and much more.
    *)

   val Y : 'a Index.t Tie.t
   (** Fixpoint tier to support recursive datatypes. *)

   (** == SUPPORT FOR FUNCTIONS == *)

   val --> : 'a Index.t * 'b Index.t -> ('a -> 'b) Index.t

   (** == SUPPORT FOR EXCEPTIONS == *)

   val exn : Exn.t Index.t
   (** Universal type-index for exceptions. *)

   val regExn : 'a Index.s -> ('a, Exn.t) Emb.t Effect.t
   (** Registers a handler for exceptions. *)

   (** == SUPPORT FOR TYPES WITH IDENTITY == *)

   val array : 'a Index.t -> 'a Array.t Index.t
   val refc : 'a Index.t -> 'a Ref.t Index.t

   (** == SUPPORT FOR FUNCTIONAL AGGREGATE TYPES == *)

   val vector : 'a Index.t -> 'a Vector.t Index.t

   (** == SUPPORT FOR ARBITRARY INTEGERS, WORDS, AND REALS == *)

   val largeInt  : LargeInt.t  Index.t
   val largeReal : LargeReal.t Index.t
   val largeWord : LargeWord.t Index.t

   (** == SUPPORT FOR BINARY DATA == *)

   val word8  : Word8.t  Index.t
   val word16 : Word16.t Index.t
   val word32 : Word32.t Index.t
   val word64 : Word64.t Index.t

   (** == SUPPORT FOR SOME BUILT-IN TYPE CONSTRUCTORS == *)

   val list : 'a Index.t -> 'a List.t Index.t

   (** == SUPPORT FOR SOME BUILT-IN BASE TYPES == *)

   val bool   : Bool.t   Index.t
   val char   : Char.t   Index.t
   val int    : Int.t    Index.t
   val real   : Real.t   Index.t
   val string : String.t Index.t
   val word   : Word.t   Index.t
end
