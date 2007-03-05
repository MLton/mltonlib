(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {BOOL} signature. *)
signature BOOL = sig
   datatype bool = datatype Bool.bool

   type t = Bool.t
   (** Convenience alias. *)

   val not : t UnOp.t
   (** Logical negation. *)

   val isTrue : t UnPr.t
   (** {isTrue x = x = true} *)

   val isFalse : t UnPr.t
   (** {isFalse x = x = false} *)

   (** == Concepts == *)

   include BOUNDED where type bounded = t
   include ORDERED where type ordered = t
   include SCANNABLE where type scannable = t
   include STRINGABLE where type stringable = t
end
