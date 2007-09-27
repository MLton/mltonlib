(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for functional random value generators.  The design is
 * based on the [http://www.cs.chalmers.se/~rjmh/QuickCheck/ QuickCheck]
 * library by Koen Claessen and John Hughes.
 *)
signature RANDOM_GEN = sig
   structure RNG : RNG

   type 'a t

   val generate : Int.t -> RNG.t -> 'a t -> 'a

   val lift : (RNG.t -> 'a) -> 'a t

   include MONAD_CORE where type 'a monad = 'a t

   structure Monad : MONAD where type 'a monad = 'a t

   val promote : ('a -> 'b t) -> ('a -> 'b) t

   val Y : 'a t Tie.t

   val variant : Word.t -> 'a t UnOp.t
   val mapUnOp : ('a, 'b) Iso.t -> 'b t UnOp.t -> 'a t UnOp.t

   val sized : (Int.t -> 'a t) -> 'a t
   val resize : Int.t UnOp.t -> 'a t UnOp.t

   val elements : 'a List.t -> 'a t
   val oneOf : 'a t List.t -> 'a t
   val frequency : (Int.t * 'a t) List.t -> 'a t

   val inRange : ('b Sq.t -> 'b t) -> ('a, 'b) Iso.t -> 'a Sq.t -> 'a t

   val  intInRange :  Int.t Sq.t ->  Int.t t
   val realInRange : Real.t Sq.t -> Real.t t
   val wordInRange : Word.t Sq.t -> Word.t t

   val bool : Bool.t t
   val word8 : Word8.t t

   val bits : Int.t -> IntInf.t t

   val list : 'a t -> Int.t -> 'a List.t t
end
