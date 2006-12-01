(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with compare functions or orderings. *)
signature CMP = sig
   type 'a t = 'a Sq.t -> Order.t
   (**
    * Type of compare functions or orderings (e.g. {Int.compare,
    * String.compare, ...}).
    *)

   val map : ('b -> 'a) -> 'a t -> 'b t
   (** Changes the domain of an ordering. *)

   val mkRelOps : 'a t -> {<  : 'a BinPr.t, <= : 'a BinPr.t,
                           >  : 'a BinPr.t, >= : 'a BinPr.t,
                           == : 'a BinPr.t, != : 'a BinPr.t}
   (** Given an ordering, returns a record of relational operators. *)

   val max : 'a t -> 'a BinOp.t
   (**
    * Given an ordering, returns a function that returns the greater of
    * its arguments.
    *)

   val min : 'a t -> 'a BinOp.t
   (**
    * Given an ordering, returns a function that returns the lesser of its
    * arguments.
    *)
end
