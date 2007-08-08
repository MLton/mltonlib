(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the closed representation types of generics.
 *)
signature CLOSED_REP = sig
   type  'a      t   (** Type of complete representations. *)
   type  'a      s   (** Type of incomplete sum representations. *)
   type ('a, 'k) p   (** Type of incomplete product representations. *)
end
