(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for functional random number generators (RNG).
 *)
signature RNG = sig
   type t
   (** The type of RNG state. *)

   structure Seed : sig
      type t
      (** The type of RNG seed. *)

      val fromWord : Word.t -> t
      (** Creates a seed from a given word. *)
   end

   val make : Seed.t -> t
   (** Makes a RNG state given an initial seed. *)

   val value : t -> Word.t
   (** Extracts the current random word from the state. *)

   val next : t UnOp.t
   (** Computes the next state. *)

   val split : Word.t -> t UnOp.t
   (** Computes a new RNG state based on the given state and word index. *)

   val maxValue : Word.t
   (** The range of generated random words is {{0w0, ..., maxValue}}. *)
end
