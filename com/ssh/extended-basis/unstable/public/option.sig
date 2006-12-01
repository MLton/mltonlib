(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {OPTION} signature. *)
signature OPTION = sig
   include OPTION

   type 'a t = 'a option
   (** Convenience alias. *)

   val isNone : 'a t UnPr.t
   (** Returns {true} if given option is {NONE}; otherwise returns {false}. *)
end
