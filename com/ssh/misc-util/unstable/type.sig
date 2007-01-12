(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A generic signature for nominal type-indexed values.
 *
 * This signature differs from the {STRUCTURAL_TYPE} signature in that an
 * implementations of this signature can make use of nominal data (names
 * of record labels and datatype constructors) as well as can distinguish
 * between complete and incomplete sums, records and tuples.  The
 * additional data makes it possible to, for example, convert an arbitrary
 * SML value to a textual presentation that matches the syntax of SML.
 * Indeed, a type-index specified using the combinators of this signature
 * is essentially a transliteration of the SML type.
 *)

signature TYPE = sig
   type 'a t       (** Type of complete type-indices. *)
   type 'a s       (** Type of incomplete sum type-indices. *)
   type ('a, 'k) p (** Type of incomplete product type-indices. *)

   (** == SUPPORT FOR USER-DEFINED TYPES == *)

   val iso : 'b t -> ('a, 'b) Iso.t -> 'a t
   (**
    * Given a type-index {'b t} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a t}.  The purpose of {iso} is to
    * support user-defined types.
    *)

   val isoProduct : ('b, 'k) p -> ('a, 'b) Iso.t -> ('a, 'k) p
   (**
    * Given a type-index {('b, 'k) p} and an isomorphism between {'a}
    * and {'b}, returns a type-index {('a, 'k) p}.
    *)

   val isoSum : 'b s -> ('a, 'b) Iso.t -> 'a s
   (**
    * Given a type-index {'b s} and an isomorphism between {'a} and
    * {'b}, returns a type-index {'a s}.
    *)

   (** == SUPPORT FOR TUPLES AND RECORDS == *)

   val *` : ('a, 'k) p * ('b, 'k) p -> (('a, 'b) Product.t, 'k) p
   (**
    * Given type-indices for fields of type {'a} and {'b} of the same
    * kind {'k} (tuple or record), returns a type-index for the product
    * {('a, 'b) product}.
    *)

   val T : 'a t -> ('a, TypeSupport.tuple) p
   (** Specifies a field of a tuple. *)

   val R : TypeSupport.label -> 'a t -> ('a, TypeSupport.record) p
   (** Specifies a field of a record. *)

   val tuple  : ('a, TypeSupport.tuple) p -> 'a t
   (** Specifies a tuple. *)

   val record : ('a, TypeSupport.record) p -> 'a t
   (** Specifies a record. *)

   (** == SUPPORT FOR DATATYPES == *)

   val +` : 'a s * 'b s -> (('a, 'b) Sum.t) s
   (**
    * Given type-indices for variants of type {'a} and {'b}, returns a
    * type-index for the sum {('a, 'b) sum}.
    *)

   val C0 : TypeSupport.constructor -> Unit.t s
   (** Specifies a nullary constructor. *)

   val C1 : TypeSupport.constructor -> 'a t -> 'a s
   (** Specifies a unary constructor. *)

   val data : 'a s -> 'a t
   (** Specifies a complete datatype. *)

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

   val regExn : 'a s -> ('a, Exn.t) Emb.t Effect.t
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
