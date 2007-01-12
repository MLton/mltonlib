(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Signature for functional random number generators.
 *)

signature RNG = sig
   type t
   (** The type of random number generator state or seed. *)

   val value : t -> Word.t
   (** Extracts the current random word from the seed. *)

   val next : t UnOp.t
   (** Computes the next seed. *)

   val split : Word.t -> t UnOp.t
   (** Computes a new seed based on the given seed and word index. *)

   val maxValue : Word.t
   (** The range of generated random words is {{0w0, ..., maxValue}}. *)
end
