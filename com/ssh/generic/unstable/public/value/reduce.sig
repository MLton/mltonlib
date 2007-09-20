(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A generic for making reduce operations.
 *
 * Examples:
 *
 *> - val sum = makeReduce 0 op + id int list ;
 *> val sum = fn : Int.t List.t -> Int.t
 *> - sum [1, 2, 3] ;
 *> val it = 6 : Int.t
 *
 *> - val count = makeReduce 0 op + (const 1) real list ;
 *> val count = fn : Real.t List.t -> Int.t
 *> - count [1.0, 4.0, 6.0] ;
 *> val it = 3 : Int.t
 *
 *> - makeReduce 0 op + id int (fn t => tuple (T t *` T int *` T t))
 *> = (1 & 3 & 7) ;
 *> val it = 8 : Int.t
 *
 * This design is experimental.
 *)
signature REDUCE = sig
   structure ReduceRep : OPEN_REP

   val makeReduce :
       'r
       -> 'r BinOp.t
       -> ('a -> 'r)
       -> ('a, 'x) ReduceRep.t
       -> (('a, 'x) ReduceRep.t -> ('b, 'y) ReduceRep.t)
       -> 'b -> 'r
   (** Creates a reduce operation. *)
end

signature REDUCE_CASES = sig
   include OPEN_CASES REDUCE
   sharing Rep = ReduceRep
end
