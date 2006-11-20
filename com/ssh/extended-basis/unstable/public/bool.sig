(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Extended {BOOL} signature. *)
signature BOOL = sig
   include BOOL

   type t = bool
   (** Convenience alias. *)

   val isTrue : t UnPr.t
   (** {isTrue x = x = true} *)

   val isFalse : t UnPr.t
   (** {isFalse x = x = false} *)

   val equal : t BinPr.t
   (** Equivalent to {op =}. *)

   val compare : t Cmp.t
   (** An ordering on booleans.  {false} is defined less than {true}. *)
end
