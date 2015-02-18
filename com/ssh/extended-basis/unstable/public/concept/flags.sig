(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * An extension and generalization of the {BIT_FLAGS} signature.
 *
 * Like {BIT_FLAGS}, the {FLAGS} signature defines a generic set of
 * operations on an abstract representation of flags.  It is typically
 * included as part of the interface of substructures which provide a set
 * of options.
 *
 * Unlike {BIT_FLAGS}, the {FLAGS} signature allows one to specify an
 * associated {flags_word} type.  Ignoring the additional specifications
 * of {FLAGS}, the {BIT_FLAGS} signature could be specified as:
 *
 *> signature BIT_FLAGS = FLAGS where type flags_word = SysWord.word
 *)
signature FLAGS = sig
   eqtype flags

   (** == Representation == *)

   type flags_word
   (** This is usually the representation type of {flags}. *)

   val fromWord : flags_word -> flags
   (** Converts a {flags_word} to a {flags}. *)

   val toWord : flags -> flags_word
   (** Converts a {flags} to a {flags_word}. *)

   val isoWord : (flags, flags_word) Iso.t
   (** This is always equivalent to {(toWord, fromWord)}. *)

   (** == Constants == *)
   
   val none: flags
   (** The empty set. *)

   val all : flags
   (** The union of all flags. *)

   (** == Set Operations == *)

   val clear : flags BinOp.t
   (** {clear (f1, f2)} is the set of flags in {f2} that are not in {f1}. *)

   val flags : flags List.t -> flags
   (** The union of flags. *)

   val intersect : flags List.t -> flags
   (** The intersection of flags.  As a special case, {intersect [] = all}. *)

   (** == Predicates == *)

   val allSet : flags BinPr.t
   (** {allSet (f1, f2)} is true iff all of the flags in {f1} are in {f2}. *)

   val anySet : flags BinPr.t
   (** {anySet (f1, f2)} is true iff any of the flags in {f1} is in {f2}. *)
end
