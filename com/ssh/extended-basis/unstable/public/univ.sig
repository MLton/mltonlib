(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Signature for a basic universal type. *)
signature UNIV = sig
   type t

   val newIso : ('a, t) Iso.t Thunk.t
   val newEmb : ('a, t) Emb.t Thunk.t
end
