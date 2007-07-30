(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** A general purpose sum type. *)
signature SUM = sig
   datatype ('a, 'b) sum = INL of 'a | INR of 'b
   type ('a, 'b) t = ('a, 'b) sum

   exception Sum

   (** == Operations == *)

   val swap : ('a, 'b) t -> ('b, 'a) t

   val isL : ('a, 'b) t UnPr.t
   val isR : ('a, 'b) t UnPr.t

   val getL : ('a, 'b) t -> 'a UnOp.t
   val getR : ('a, 'b) t -> 'b UnOp.t

   val out : ('a, 'a) t -> 'a
   val outL : ('a, 'b) t -> 'a
   val outR : ('a, 'b) t -> 'b

   (** == HOFs == *)

   val sum : ('a -> 'c) * ('b -> 'c) -> ('a, 'b) t -> 'c

   val app : 'a Effect.t * 'b Effect.t -> ('a, 'b) t Effect.t
   val appL : 'a Effect.t -> ('a, 'b) t Effect.t
   val appR : 'b Effect.t -> ('a, 'b) t Effect.t

   val map : ('a -> 'c) * ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
   val mapL : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
   val mapR : ('b -> 'd) -> ('a, 'b) t -> ('a, 'd) t
   val mapLR : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t

   val equal : 'a BinPr.t * 'b BinPr.t -> ('a, 'b) t BinPr.t
   val collate : 'a Cmp.t * 'b Cmp.t -> ('a, 'b) t Cmp.t

   (** == Generic Programming == *)

   val iso : ('a, 'c) Iso.t * ('b, 'd) Iso.t -> (('a, 'b) t, ('c, 'd) t) Iso.t
   (** Lifts isos between elements to an iso between sums. *)
end
