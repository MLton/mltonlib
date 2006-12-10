(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
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
