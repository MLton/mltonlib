(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Extended {SUBSTRING} signature. *)
signature SUBSTRING = sig
   include SUBSTRING

   type t = substring
   (** Convenience alias. *)

   val length : t -> Int.t
   (**
    * Returns the size of the given substring.  This is equivalent to
    * {size}.
    *)
end
