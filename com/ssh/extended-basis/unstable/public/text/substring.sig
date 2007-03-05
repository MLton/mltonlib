(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {SUBSTRING} signature. *)
signature SUBSTRING = sig
   include BASIS_SUBSTRING

   type t = substring
   (** Convenience alias. *)

   val length : t -> Int.t
   (**
    * Returns the size of the given substring.  This is equivalent to
    * {size}.
    *)
end
