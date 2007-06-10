(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for type-indexed values based on a generic representation
 * of datatypes.
 *)
signature CLOSED_GENERIC = sig
   structure Rep : CLOSED_GENERIC_REP

   (** == SUPPORT FOR USER-DEFINED TYPES == *)

   val iso : 'b Rep.t -> ('a, 'b) Iso.t -> 'a Rep.t
   (**
    * Given a type-index {'b Rep.t} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a Rep.t}.  The purpose of {iso} is to
    * support user-defined types.
    *)

   val isoProduct : ('b, 'k) Rep.p -> ('a, 'b) Iso.t -> ('a, 'k) Rep.p
   (**
    * Given a type-index {('b, 'k) Rep.p} and an isomorphism between
    * {'a} and {'b}, returns a type-index {('a, 'k) Rep.p}.
    *)

   val isoSum : 'b Rep.s -> ('a, 'b) Iso.t -> 'a Rep.s
   (**
    * Given a type-index {'b Rep.s} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a Rep.s}.
    *)

   (** == SUPPORT FOR TUPLES AND RECORDS == *)

   val *` :
       ('a, 'k) Rep.p * ('b, 'k) Rep.p -> (('a, 'b) Product.t, 'k) Rep.p
   (**
    * Given type-indices for fields of type {'a} and {'b} of the same kind
    * {'k} (tuple or record), returns a type-index for the product {('a,
    * 'b) Product.t}.
    *)

   val T : 'a Rep.t -> ('a, Generics.Tuple.t) Rep.p
   (** Specifies a field of a tuple. *)

   val R : Generics.Label.t -> 'a Rep.t -> ('a, Generics.Record.t) Rep.p
   (** Specifies a field of a record. *)

   val tuple  : ('a, Generics.Tuple.t) Rep.p -> 'a Rep.t
   (** Specifies a tuple. *)

   val record : ('a, Generics.Record.t) Rep.p -> 'a Rep.t
   (** Specifies a record. *)

   (** == SUPPORT FOR DATATYPES == *)

   val +` : 'a Rep.s * 'b Rep.s -> (('a, 'b) Sum.t) Rep.s
   (**
    * Given type-indices for variants of type {'a} and {'b}, returns a
    * type-index for the sum {('a, 'b) Sum.t}.
    *)

   val C0 : Generics.Con.t -> Unit.t Rep.s
   (** Specifies a nullary constructor. *)

   val C1 : Generics.Con.t -> 'a Rep.t -> 'a Rep.s
   (** Specifies a unary constructor. *)

   val data : 'a Rep.s -> 'a Rep.t
   (** Specifies a complete datatype. *)

   val unit : Unit.t Rep.t
   (**
    * Type-index for the {unit} type.  Using {unit} and {+} one can
    * actually encode {bool}, {word}, and much more.
    *)

   val Y : 'a Rep.t Tie.t
   (** Fixpoint tier to support recursive datatypes. *)

   (** == SUPPORT FOR FUNCTIONS == *)

   val --> : 'a Rep.t * 'b Rep.t -> ('a -> 'b) Rep.t

   (** == SUPPORT FOR EXCEPTIONS == *)

   val exn : Exn.t Rep.t
   (** Universal type-index for exceptions. *)

   val regExn : 'a Rep.s -> ('a, Exn.t) Emb.t Effect.t
   (** Registers a handler for exceptions. *)

   (** == SUPPORT FOR TYPES WITH IDENTITY == *)

   val array : 'a Rep.t -> 'a Array.t Rep.t
   val refc : 'a Rep.t -> 'a Ref.t Rep.t

   (** == SUPPORT FOR FUNCTIONAL AGGREGATE TYPES == *)

   val vector : 'a Rep.t -> 'a Vector.t Rep.t

   (** == SUPPORT FOR ARBITRARY INTEGERS, WORDS, AND REALS == *)

   val largeInt  : LargeInt.t  Rep.t
   val largeReal : LargeReal.t Rep.t
   val largeWord : LargeWord.t Rep.t

   (** == SUPPORT FOR BINARY DATA == *)

   val word8  : Word8.t  Rep.t
(* val word16 : Word16.t Rep.t (* Word16 not provided by SML/NJ *) *)
   val word32 : Word32.t Rep.t
   val word64 : Word64.t Rep.t

   (** == SUPPORT FOR SOME BUILT-IN TYPE CONSTRUCTORS == *)

   val list : 'a Rep.t -> 'a List.t Rep.t

   (** == SUPPORT FOR SOME BUILT-IN BASE TYPES == *)

   val bool   : Bool.t   Rep.t
   val char   : Char.t   Rep.t
   val int    : Int.t    Rep.t
   val real   : Real.t   Rep.t
   val string : String.t Rep.t
   val word   : Word.t   Rep.t
end
