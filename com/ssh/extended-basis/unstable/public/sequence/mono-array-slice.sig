(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {MONO_ARRAY_SLICE} signature. *)
signature MONO_ARRAY_SLICE = sig
   include BASIS_MONO_ARRAY_SLICE

   type t sharing type t = slice
   (** Convenience alias. *)
end
