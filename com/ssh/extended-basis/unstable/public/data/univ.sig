(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a basic, non-structural, universal type.
 *
 * It is important to understand that the universal type is not
 * structural.  Consider the following code:
 *
 *> val a : (Int.t, Univ.t) Emb.t = Univ.newEmb ()
 *> val b : (Int.t, Univ.t) Emb.t = Univ.newEmb ()
 *
 *> val x : Univ.t = Emb.to a 5
 *
 * Now {Emb.from a x} is {SOME 5}, but {Emb.from b x} is {NONE}.  The
 * embeddings {a} and {b} have different identity.  Each time {newEmb} or
 * {newIso} is called, a new identity is created.
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
