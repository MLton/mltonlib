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

   val droplr : char UnPr.t -> t UnOp.t
   (** {droplr p} is equivalent to {dropl p o dropr p}. *)

   val extendl : char UnPr.t -> t UnOp.t
   val extendr : char UnPr.t -> t UnOp.t
   (**
    * These extend the substring with characters satisfying the predicate
    * as far as possible within the underlying string either to the left
    * or to the right.  For example,
    *
    *> "aa" = string o extendr (eq #"a") |< substring ("aabb", 0, 0)
    *
    * and
    *
    *> "aabb" = string o extendr (const true) |< substring ("aabb", 0, 0)
    *)
end
