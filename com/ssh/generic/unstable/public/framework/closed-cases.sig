(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for closed structural cases.
 *)
signature CLOSED_CASES = sig
   structure Rep : CLOSED_REP

   (** == Support for User-Defined Types == *)

   val iso : 'b Rep.t -> ('a, 'b) Iso.t -> 'a Rep.t
   (**
    * Given a representation {'b Rep.t} and an isomorphism between {'a}
    * and {'b}, returns a representation {'a Rep.t}.  The purpose of {iso}
    * is to support user-defined types.
    *)

   val isoProduct : ('b, 'k) Rep.p -> ('a, 'b) Iso.t -> ('a, 'k) Rep.p
   (**
    * Given a representation {('b, 'k) Rep.p} and an isomorphism between
    * {'a} and {'b}, returns a representation {('a, 'k) Rep.p}.
    *)

   val isoSum : 'b Rep.s -> ('a, 'b) Iso.t -> 'a Rep.s
   (**
    * Given a representation {'b Rep.s} and an isomorphism between {'a}
    * and {'b}, returns a representation {'a Rep.s}.
    *)

   (** == Support for Tuples and Records == *)

   val *` : ('a, 'k) Rep.p * ('b, 'k) Rep.p -> (('a, 'b) Product.t, 'k) Rep.p
   (**
    * Given representations for fields of type {'a} and {'b} of the same
    * kind {'k} (tuple or record), returns a representation for the
    * product {('a, 'b) Product.t}.
    *)

   val T : 'a Rep.t -> ('a, Generics.Tuple.t) Rep.p
   (** Specifies a field of a tuple. *)

   val R : Generics.Label.t -> 'a Rep.t -> ('a, Generics.Record.t) Rep.p
   (** Specifies a field of a record. *)

   val tuple  : ('a, Generics.Tuple.t) Rep.p -> 'a Rep.t
   (** Specifies a tuple. *)

   val record : ('a, Generics.Record.t) Rep.p -> 'a Rep.t
   (** Specifies a record. *)

   (** == Support for Datatypes == *)

   val +` : 'a Rep.s * 'b Rep.s -> ('a, 'b) Sum.t Rep.s
   (**
    * Given representations for variants of type {'a} and {'b}, returns a
    * representation for the sum {('a, 'b) Sum.t}.
    *)

   val C0 : Generics.Con.t -> Unit.t Rep.s
   (** Specifies a nullary constructor. *)

   val C1 : Generics.Con.t -> 'a Rep.t -> 'a Rep.s
   (** Specifies a unary constructor. *)

   val data : 'a Rep.s -> 'a Rep.t
   (** Specifies a complete datatype. *)

   val unit : Unit.t Rep.t
   (**
    * Representation for the {unit} type.  Using {unit} and {+} one can
    * actually encode {bool}, {word}, and much more.
    *)

   val Y : 'a Rep.t Tie.t
   (** Fixed-point tier to support recursive datatypes. *)

   (** == Support for Functions == *)

   val --> : 'a Rep.t * 'b Rep.t -> ('a -> 'b) Rep.t

   (** == Support for Exceptions == *)

   val exn : Exn.t Rep.t
   (** Universal representation for exceptions. *)

   val regExn0 : Generics.Con.t -> (Exn.t * (Exn.t -> Unit.t Option.t)) Effect.t
   (** Registers a nullary exception constructor. *)

   val regExn1 : Generics.Con.t -> 'a Rep.t -> ('a, Exn.t) Emb.t Effect.t
   (** Registers an unary exception constructor. *)

   (** == Support for Types With Identity == *)

   val array : 'a Rep.t -> 'a Array.t Rep.t
   val refc : 'a Rep.t -> 'a Ref.t Rep.t

   (** == Support for Functional Aggregate Types == *)

   val vector : 'a Rep.t -> 'a Vector.t Rep.t

   (** == Support for Arbitrary Integers, Words, And Reals == *)

   val fixedInt : FixedInt.t Rep.t
   val largeInt : LargeInt.t Rep.t

   val largeReal : LargeReal.t Rep.t
   val largeWord : LargeWord.t Rep.t

   (** == Support for Binary Data == *)

   val word8  : Word8.t  Rep.t
   val word32 : Word32.t Rep.t
(*
   val word64 : Word64.t Rep.t
*)

   (** == Support for Some Built-In Type Constructors == *)

   val list : 'a Rep.t -> 'a List.t Rep.t

   (** == Support for Some Built-In Base Types == *)

   val bool   : Bool.t   Rep.t
   val char   : Char.t   Rep.t
   val int    : Int.t    Rep.t
   val real   : Real.t   Rep.t
   val string : String.t Rep.t
   val word   : Word.t   Rep.t
end
