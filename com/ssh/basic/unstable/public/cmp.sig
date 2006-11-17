(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with compare functions. *)
signature CMP = sig
   type 'a t = 'a Sq.t -> Order.t
   (** Type of compare functions (e.g. {Int.compare, String.compare, ...}). *)

   val mkRelOps : 'a t -> {<  : 'a BinPr.t, <= : 'a BinPr.t,
                           >  : 'a BinPr.t, >= : 'a BinPr.t,
                           == : 'a BinPr.t, != : 'a BinPr.t}
   (** Returns a record of relational operators given a compare function. *)
end
