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
 *> - val sum = makeReduce list int 0 op + id ;
 *> val sum = fn : Int.t List.t -> Int.t
 *> - sum [1, 2, 3] ;
 *> val it = 6 : Int.t
 *
 *> - val count = makeReduce list real 0 op + (const 1) ;
 *> val count = fn : Real.t List.t -> Int.t
 *> - count [1.0, 4.0, 6.0] ;
 *> val it = 3 : Int.t
 *
 *> - makeReduce (fn t => tuple (T t *` T int *` T t)) int 0 op + id
 *> = (1 & 3 & 7) ;
 *> val it = 8 : Int.t
 *
 * This design is experimental.
 *)
signature REDUCE = sig
   structure ReduceRep : OPEN_REP

   val makeReduce :
       (('a, 'x) ReduceRep.t -> ('b, 'y) ReduceRep.t)
       -> ('a, 'x) ReduceRep.t
       -> 'r
       -> 'r BinOp.t
       -> ('a -> 'r)
       -> 'b -> 'r
   (** Creates a reduce operation. *)
end

signature REDUCE_CASES = sig
   include CASES REDUCE
   sharing Open.Rep = ReduceRep
end

signature WITH_REDUCE_DOM = CASES
