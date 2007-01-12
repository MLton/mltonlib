(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A generic signature for "structural" type-indexed values.
 *
 * The intended usage of this signature includes type-indexed values such
 * as
 * - generic (as opposed to polymorphic) equality (eq),
 * - generic hashing (hash),
 * - generic ordering (compare),
 * - generic generation of random values (arbitrary), and
 * - generic serialization (pickle).
 *
 * In general, this signature is intended for type-indexed values whose
 * domain is (almost all of) SML types.  Conversely, this signature is not
 * designed for domain specific type-indexed values whose domain does not
 * directly match the SML type system.
 *
 * As you can see, of the integer types, only {int} and {largeInt} are
 * specialized, while a much wider range of word types is supported.  The
 * intention is that the integer types should not be used for things such
 * as serialization.  If sized integer types are really needed, they are
 * to be implemented through the corresponding sized word types.
 *)

signature STRUCTURAL_TYPE = sig
   type 'a t
   (** Type of type-indices. *)

   (** == SUPPORT FOR USER-DEFINED TYPES == *)

   val iso : 'b t -> ('a, 'b) Iso.t -> 'a t
   (**
    * Given an isomorphism between {'a} and {'b} and a type-index for
    * {'b}, returns a type-index for {'a}.  The purpose of {iso} is to
    * support user-defined types.
    *)

   (** == SUPPORT FOR TUPLES AND RECORDS == *)

   val *` : 'a t * 'b t -> ('a, 'b) Product.t t
   (**
    * Given type-indices for fields of type {'a} and {'b}, returns a
    * type-index for the product {('a, 'b) product}.
    *)

   (** == SUPPORT FOR DATATYPES == *)

   val +` : 'a t * 'b t -> ('a, 'b) Sum.t t
   (**
    * Given type-indices for variants of type {'a} and {'b}, returns a
    * type-index for the sum {('a, 'b) sum}.
    *)

   val unit : Unit.t t
   (**
    * Type-index for the {unit} type.  Using {unit} and {+} one can
    * actually encode {bool}, {word}, and much more.
    *)

   val Y : 'a t Tie.t
   (** Fixpoint tier to support recursive datatypes. *)

   (** == SUPPORT FOR FUNCTIONS == *)

   val --> : 'a t * 'b t -> ('a -> 'b) t

   (** == SUPPORT FOR EXCEPTIONS == *)

   val exn : Exn.t t
   (** Universal type-index for exceptions. *)

   val regExn : 'a t -> ('a, Exn.t) Emb.t Effect.t
   (** Registers a handler for exceptions. *)

   (** == SUPPORT FOR TYPES WITH IDENTITY == *)

   val array : 'a t -> 'a Array.t t
   val refc  : 'a t -> 'a Ref.t   t

   (** == SUPPORT FOR FUNCTIONAL AGGREGATE TYPES == *)

   val vector : 'a t -> 'a Vector.t t

   (** == SUPPORT FOR ARBITRARY INTEGERS, WORDS, AND REALS == *)

   val largeInt  : LargeInt.t  t
   val largeReal : LargeReal.t t
   val largeWord : LargeWord.t t

   (** == SUPPORT FOR BINARY DATA == *)

   val word8  : Word8.t  t
   val word16 : Word16.t t
   val word32 : Word32.t t
   val word64 : Word64.t t

   (** == SUPPORT FOR SOME BUILT-IN TYPE CONSTRUCTORS == *)

   val list : 'a t -> 'a List.t t

   (** == SUPPORT FOR SOME BUILT-IN BASE TYPES == *)

   val bool   : Bool.t   t
   val char   : Char.t   t
   val int    : Int.t    t
   val real   : Real.t   t
   val string : String.t t
   val word   : Word.t   t
end
