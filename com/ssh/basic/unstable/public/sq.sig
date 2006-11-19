(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Utilities for dealing with "squares", that is, pairs whose both
 * elements are of the same type.
 *)
signature SQ = sig
   type 'a t = 'a * 'a
   (** The type of "squares". *)

   val mk : 'a -> 'a t
   (** {mk x = (x, x)}. *)

   val map : ('a -> 'b) -> 'a t -> 'b t
   (** {map f (x, y) = (f x, f y)}. *)
end
