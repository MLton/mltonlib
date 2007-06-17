(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature LAYER_GENERIC_REP_DOM = sig
   structure Outer : OPEN_GENERIC_REP
   structure Rep : CLOSED_GENERIC_REP
end

functor LayerGenericRep (Arg : LAYER_GENERIC_REP_DOM) : 
   OPENED_GENERIC_REP
      where type 'a This.Closed.t = 'a Arg.Rep.t
      where type 'a This.Closed.s = 'a Arg.Rep.s
      where type ('a, 'k) This.Closed.p = ('a, 'k) Arg.Rep.p =
struct
   structure Inner = OpenGenericRep (Arg.Rep)
   structure Joined = JoinGenericReps (open Arg structure Inner = Inner)
   open Joined
   structure This = struct
      structure Rep = Joined
      structure Closed = Arg.Rep
      fun getT ? = Inner.This.getT (Arg.Outer.getT ?)
      fun getS ? = Inner.This.getS (Arg.Outer.getS ?)
      fun getP ? = Inner.This.getP (Arg.Outer.getP ?)
      fun mapT ? = Arg.Outer.mapT (Inner.This.mapT ?)
      fun mapS ? = Arg.Outer.mapS (Inner.This.mapS ?)
      fun mapP ? = Arg.Outer.mapP (Inner.This.mapP ?)
   end
end

functor LayerGeneric (Arg : LAYER_GENERIC_DOM) :
   OPEN_GENERIC
      where type ('a, 'x) Rep.t = ('a, 'x) Arg.Result.t
      where type ('a, 'x) Rep.s = ('a, 'x) Arg.Result.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Result.p =
struct
   fun op0 outer this x = outer (this, x)
   fun op1 outer this x2y a = outer (fn (_, x) => (this a, x2y x)) a
   fun op2 outer this xy2z ab =
       outer (fn ((_, x), (_, y)) => (this ab, xy2z (x, y))) ab
   fun morph outer this f b = outer (fn (_, y) => fn i => (this b i, f y i)) b
   val t = op1
   fun r outer this lx2y l a =
       outer (fn l => fn (_, x) => (this l a, lx2y l x)) l a
   fun c0 outer l2s l2x = outer (Pair.map (l2s, l2x) o Sq.mk)
   val c1 = r
   fun y outer x y = outer (Tie.tuple2 (x, y))
   fun re outer this ex a =
       outer (fn (_, x) => fn e => (this a e : Unit.t ; ex x e : Unit.t)) a
   structure Rep = Arg.Result
   fun iso ? = morph Arg.Outer.iso Arg.iso ?
   fun isoProduct ? = morph Arg.Outer.isoProduct Arg.isoProduct ?
   fun isoSum ? = morph Arg.Outer.isoSum Arg.isoSum ?
   fun op *` ? = op2 Arg.Outer.*` Arg.*` ?
   fun T ? = t Arg.Outer.T Arg.T ?
   fun R ? = r Arg.Outer.R Arg.R ?
   fun tuple ? = op1 Arg.Outer.tuple Arg.tuple ?
   fun record ? = op1 Arg.Outer.record Arg.record ?
   fun op +` ? = op2 Arg.Outer.+` Arg.+` ?
   fun C0 ? = c0 Arg.Outer.C0 Arg.C0 ?
   fun C1 ? = c1 Arg.Outer.C1 Arg.C1 ?
   fun data ? = op1 Arg.Outer.data Arg.data ?
   fun unit ? = op0 Arg.Outer.unit Arg.unit ?
   fun Y ? = y Arg.Outer.Y Arg.Y ?
   fun op --> ? = op2 Arg.Outer.--> Arg.--> ?
   fun exn ? = op0 Arg.Outer.exn Arg.exn ?
   fun regExn ? = re Arg.Outer.regExn Arg.regExn ?
   fun array ? = op1 Arg.Outer.array Arg.array ?
   fun refc ? = op1 Arg.Outer.refc Arg.refc ?
   fun vector ? = op1 Arg.Outer.vector Arg.vector ?
   fun largeInt ? = op0 Arg.Outer.largeInt Arg.largeInt ?
   fun largeReal ? = op0 Arg.Outer.largeReal Arg.largeReal ?
   fun largeWord ? = op0 Arg.Outer.largeWord Arg.largeWord ?
   fun word8 ? = op0 Arg.Outer.word8 Arg.word8 ?
(* val word16 ? = op0 Arg.Outer.word16 Arg.word16 ?
   (* Word16 not provided by SML/NJ *) *)
   fun word32 ? = op0 Arg.Outer.word32 Arg.word32 ?
   fun word64 ? = op0 Arg.Outer.word64 Arg.word64 ?
   fun list ? = op1 Arg.Outer.list Arg.list ?
   fun bool ? = op0 Arg.Outer.bool Arg.bool ?
   fun char ? = op0 Arg.Outer.char Arg.char ?
   fun int ? = op0 Arg.Outer.int Arg.int ?
   fun real ? = op0 Arg.Outer.real Arg.real ?
   fun string ? = op0 Arg.Outer.string Arg.string ?
   fun word ? = op0 Arg.Outer.word Arg.word ?
end
