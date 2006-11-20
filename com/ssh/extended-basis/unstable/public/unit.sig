(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Signature for the unit type home module. *)
signature UNIT = sig
   type t = unit
   (** The unit type. *)
end
