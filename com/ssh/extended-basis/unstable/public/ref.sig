(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with references. *)
signature REF = sig
   type 'a t
   (**
    * The type of references.  This is left abstract to allow for
    * different types of references.  In particular, in addition to the
    * built-in references of SML, one could implement, in a suitable
    * extension to SML, a type of references whose various operations
    * would be guaranteed to be atomic.
    *)

   val new : 'a -> 'a t
   (** Creates a new reference to the given value. *)

   val ! : 'a t -> 'a
   (** Returns the value referred to by the given reference. *)

   val := : ('a t * 'a) Effect.t
   (** {r := v} makes the reference {r} refer to the value {v}.  *)

   val :=: : 'a t Sq.t Effect.t
   (**
    * Swaps the values referred to by the given references.  Ignoring
    * concurrency, {r1 :=: r2} is equivalent to
    *
    *> let
    *>    val v1 = !r1
    *>    val v2 = !r2
    *> in
    *>    r1 := v2
    *>  ; r2 := v1
    *> end
    *)

   val exchange : 'a t * 'a -> 'a
   (**
    * Ignoring concurrency, {exchange (r, v)} is equivalent to {!r before
    * r := v}.
    *)

   val app : 'a Effect.t -> 'a t Effect.t
   (** {app ef r} is equivalent to {ef (!r)}. *)

   val map : ('a -> 'b) -> 'a t -> 'b t
   (** {map f r} is equivalent to {new (f (!r))}. *)

   val modify : 'a UnOp.t -> 'a t Effect.t
   (** Ignoring concurrency, {modify f r} is equivalent to {r := f (!r)}. *)

   val equal : 'a t BinPr.t
   (**
    * Returns true if the given references were, in fact, created by a
    * single call to {new}.  This is equivalent to {op =}.
    *)
end
