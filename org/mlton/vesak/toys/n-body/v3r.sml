(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

infix 7 :*: :* *: :/: :/ /:
infix 6 :+: :+ +: :-: :- -:

signature SCALAR = sig
   type t
   val ~ : t UnOp.t
   val + : t BinOp.t
   val - : t BinOp.t
   val * : t BinOp.t
   val / : t BinOp.t
   structure Math : sig
      val sqrt : t UnOp.t
   end
   val fromInt : Int.t -> t
end

signature SEQ_CORE = sig
   type 'a t
   val map : ('a -> 'b) -> 'a t -> 'b t
   val selector : ('a t -> 'a) t
   val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end

signature SEQ = sig
   include SEQ_CORE
   val app : 'a Effect.t -> 'a t Effect.t
   val toList : 'a t -> 'a List.t
   val dup : 'a -> 'a t
   val zipWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   type 'a r
   val sub : ('a r t -> 'a r) -> 'a t -> 'a
   val update : ('a r t -> 'a r) -> 'a t * 'a -> 'a t
   val sumWith : ('a * 'a -> 'a) -> 'a t -> 'a
end

functor MkSeq (Core : SEQ_CORE) :> SEQ where type 'a t = 'a Core.t = struct
   open Core
   fun zipWith f (l, r) = let
      val l = map INL l
      val r = map INR r
   in
      map (fn s => f (Sum.outL (s l), Sum.outR (s r))) selector
   end
   fun app e = ignore o map e
   fun dup v = map (const v) selector
   fun toList v = foldr op :: [] v
   type 'a r = 'a ref
   fun sub f v = case map ref v of r => ! (f r)
   fun update f (v, s) = case map ref v of r => (f r := s ; map ! r)
   fun sumWith f =
       Sum.outR o foldr (fn (v, INL ()) => INR v
                          | (v, INR s) => INR (f (s, v))) (INL ())
end

signature VEC = sig
   structure Scalar : SCALAR and Seq : SEQ

   type s = Scalar.t and v = Scalar.t Seq.t

   val diag : s -> v -> v Seq.t

   val e : v Seq.t

   val ~: : v UnOp.t

   val :+: : v BinOp.t  val :+  : v * s -> v  val  +: : s * v -> v
   val :-: : v BinOp.t  val :-  : v * s -> v  val  -: : s * v -> v
   val :*: : v BinOp.t  val :*  : v * s -> v  val  *: : s * v -> v
   val :/: : v BinOp.t  val :/  : v * s -> v  val  /: : s * v -> v

   val dot : v Sq.t -> s
   val norm : v -> s
   val mag : v -> s

   val lerp : v Sq.t -> s -> v

   val normalize : v UnOp.t
end

functor Vec (structure Scalar : SCALAR and Seq : SEQ_CORE) : VEC = struct
   structure Scalar = Scalar and Seq = MkSeq (Seq)

   open Scalar Seq

   type s = Scalar.t and v = Scalar.t Seq.t

   fun diag s v = map (fn f => update f (dup s, sub f v)) selector

   val e = diag (fromInt 0) (dup (fromInt 1))

   val ~: = map Scalar.~

   local
      fun mk f =
          case zipWith f
           of vv => vv & vv o Pair.map (id, dup) & vv o Pair.map (dup, id)
   in
      val op :+: & op :+ & op +: = mk op +
      val op :-: & op :- & op -: = mk op -
      val op :*: & op :* & op *: = mk op *
      val op :/: & op :/ & op /: = mk op /
   end

   val dot = sumWith op + o op :*:
   val norm = dot o Sq.mk
   val mag = Math.sqrt o norm

   fun lerp (l, r) s = l :* (fromInt 1 - s) :+: r :* s

   fun normalize v = v :* (fromInt 1 / mag v)
end

structure XYZ : SEQ_CORE = struct
   type 'a t = {x : 'a, y : 'a, z : 'a}
   val selector : ('a t -> 'a) t = {x = #x, y = #y, z = #z}
   fun map f {x, y, z} = {x = f x, y = f y, z = f z}
   fun foldr f s {x, y, z} = f (x, f (y, f (z, s)))
end

structure V3R = Vec (structure Scalar = Real and Seq = XYZ)
