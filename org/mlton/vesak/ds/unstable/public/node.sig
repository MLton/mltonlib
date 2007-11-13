(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for an imperative singly linked list node.  This is useful
 * and possibly more convenient and efficient than a functional list when
 * implementing imperative data structures (e.g. imperative hast tables).
 *
 * Note that imperative lists may form cycles and, unless otherwise
 * specified, procedures specified in this module are not specifically
 * designed to work with cyclic lists.
 *)
signature NODE = sig
   type 'a t
   type 'a p = 'a t Option.t Ref.t

   val new : 'a -> 'a t
   val ptr : 'a p Thunk.t

   val next : 'a t -> 'a p
   val value : 'a t -> 'a

   val isEmpty : 'a p UnPr.t

   val length : 'a p -> Int.t

   val hd : 'a p -> 'a
   val tl : 'a p UnOp.t

   val push : 'a p -> 'a Effect.t
   val pop : 'a p -> 'a Option.t

   val peek : 'a p -> 'a Option.t

   val drop : 'a p Effect.t

   val find : 'a UnPr.t -> 'a p -> ('a p, 'a p) Sum.t
   val fold : ('a * 's -> 's) -> 's -> 'a p -> 's

   val toList : 'a p -> 'a List.t

   val filter : 'a UnPr.t -> 'a p UnOp.t

   val appClear : 'a Effect.t -> 'a p UnOp.t

   val insert : 'a BinPr.t -> 'a p -> 'a Effect.t
end
