(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A generic for making transform operations.
 *
 * By default, mutable values, references and arrays, are modified
 * in-place.
 *
 * Examples:
 *
 *> - makeTransform list int (fn x => x + 1) [1, 2, 3] ;
 *> val it = [2, 3, 4] : Int.t List.t
 *
 *> - makeTransform (fn t => tuple (T int *` T t)) int op ~ (1 & 3) ;
 *> val it = (1 & ~3) : (Int.t, Int.t) Product.t
 *
 * This design is experimental.
 *)
signature TRANSFORM = sig
   structure TransformRep : OPEN_REP

   val makeTransform :
       (('a, 'x) TransformRep.t -> ('b, 'y) TransformRep.t)
       -> ('a, 'x) TransformRep.t
       -> 'a UnOp.t
       -> 'b UnOp.t
   (** Creates a transform operation. *)
end

signature TRANSFORM_CASES = sig
   include CASES TRANSFORM
   sharing Open.Rep = TransformRep
end

signature WITH_TRANSFORM_DOM = HASH_CASES
