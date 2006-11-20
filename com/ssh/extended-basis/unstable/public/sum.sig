(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * A general purpose sum type.
 *)
signature SUM = sig
   datatype ('a, 'b) sum = INL of 'a | INR of 'b
   type ('a, 'b) t = ('a, 'b) sum

   exception Sum

   val sum : ('a -> 'c) * ('b -> 'c) -> ('a, 'b) t -> 'c

   val swap : ('a, 'b) t -> ('b, 'a) t

   val out : ('x, 'x) t -> 'x

   val app : 'a Effect.t * 'b Effect.t -> ('a, 'b) t Effect.t

   val map : ('a -> 'c) * ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

   val isL : ('a, 'b) t UnPr.t
   val isR : ('a, 'b) t UnPr.t

   val outL : ('a, 'b) t -> 'a
   val outR : ('a, 'b) t -> 'b

   val getL : ('a, 'b) t -> 'a UnOp.t
   val getR : ('a, 'b) t -> 'b UnOp.t

   val mapL : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
   val mapR : ('b -> 'd) -> ('a, 'b) t -> ('a, 'd) t

   val equal : 'a BinPr.t * 'b BinPr.t -> ('a, 'b) t BinPr.t

   val collate : 'a Cmp.t * 'b Cmp.t -> ('a, 'b) t Cmp.t
end
