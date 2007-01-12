(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A signature for random value generators.  The design is based on the
 * QuickCheck library by Koen Claessen and John Hughes:
 *
 *   http://www.cs.chalmers.se/~rjmh/QuickCheck/ .
 *)

signature RANDOM_GEN = sig
   include RNG

   type 'a gen = Int.t -> t -> 'a

   val lift : (t -> 'a) -> 'a gen

   val return : 'a -> 'a gen
   val >>= : 'a gen * ('a -> 'b gen) -> 'b gen

   val prj : 'b gen -> ('b -> 'a) -> 'a gen

   val promote : ('a -> 'b gen) -> ('a -> 'b) gen

   val sized : (Int.t -> 'a gen) -> 'a gen
   val resize : Int.t UnOp.t -> 'a gen UnOp.t

   val elements : 'a List.t -> 'a gen
   val oneOf : 'a gen List.t -> 'a gen
   val frequency : (Int.t * 'a gen) List.t -> 'a gen

   val inRange : ('b Sq.t -> 'b gen) -> ('a, 'b) Iso.t -> 'a Sq.t -> 'a gen

   val intInRange  : Int.t  Sq.t -> Int.t  gen
   val realInRange : Real.t Sq.t -> Real.t gen
   val wordInRange : Word.t Sq.t -> Word.t gen

   val bool : Bool.t gen

   val list : 'a gen -> Int.t -> 'a List.t gen
end
