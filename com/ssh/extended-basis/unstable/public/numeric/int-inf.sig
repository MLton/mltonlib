(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {INT_INF} signature. *)
signature INT_INF = sig
   include INTEGER

   val divMod : t Sq.t UnOp.t
   val quotRem : t Sq.t UnOp.t

   val pow : t * Int.t -> t
   val log2 : t -> Int.t

   include BITWISE SHIFTABLE

   sharing type t = bitwise = shiftable
end
