(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A generic for making reduce operations.
 *
 * This design is experimental.
 *)
signature REDUCE = sig
   structure Reduce : OPEN_GENERIC_REP

   val makeReduce :
       'r
       -> 'r BinOp.t
       -> ('a -> 'r)
       -> ('a, 'x) Reduce.t
       -> (('a, 'x) Reduce.t -> ('b, 'y) Reduce.t)
       -> 'b -> 'r
   (** Creates a reduce operation. *)
end

signature REDUCE_GENERIC = sig
   include OPEN_GENERIC REDUCE
   sharing Rep = Reduce
end
