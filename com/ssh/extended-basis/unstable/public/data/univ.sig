(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a basic universal type.
 *
 * See also: [http://mlton.org/UniversalType]
 *)
signature UNIV = sig
   type t
   (** The universal type. *)

   exception Univ
   (** Raised in case of a mismatched projection. *)

   val newIso : ('a, t) Iso.t Thunk.t
   (**
    * Creates a new embedding of an arbitrary type {'a} to the universal
    * type {t} and returns it as an isomorphism whose projection function
    * is partial.  The projection function raises {Univ} in case of a
    * mismatch.
    *)

   val newEmb : ('a, t) Emb.t Thunk.t
   (**
    * Creates a new embedding of an arbitrary type {'a} to the universal
    * type {t}.
    *)
end
