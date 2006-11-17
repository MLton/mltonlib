(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Signature for a basic universal type. *)
signature UNIV = sig
   type t

   val newIso : ('a, t) Iso.t Thunk.t
   val newEmb : ('a, t) Emb.t Thunk.t
end
