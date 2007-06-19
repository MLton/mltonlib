(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor LayerGenericRep (Arg : LAYER_GENERIC_REP_DOM) :>
   LAYERED_GENERIC_REP
      where type  'a      Closed.t =  'a      Arg.Closed.t
      where type  'a      Closed.s =  'a      Arg.Closed.s
      where type ('a, 'k) Closed.p = ('a, 'k) Arg.Closed.p

      where type ('a,     'x) Outer.t = ('a,     'x) Arg.Outer.t
      where type ('a,     'x) Outer.s = ('a,     'x) Arg.Outer.s
      where type ('a, 'k, 'x) Outer.p = ('a, 'k, 'x) Arg.Outer.p =
struct
   open Arg
   structure Inner = struct
      type ('a,     'x) t =  'a      Closed.t * 'x
      type ('a,     'x) s =  'a      Closed.s * 'x
      type ('a, 'k, 'x) p = ('a, 'k) Closed.p * 'x
      val mkT = Fn.id
      val mkS = Fn.id
      val mkP = Fn.id
      val mkY = Tie.tuple2
      val getT = Pair.snd
      val getS = Pair.snd
      val getP = Pair.snd
      val mapT = Pair.mapSnd
      val mapS = Pair.mapSnd
      val mapP = Pair.mapSnd
   end
   structure Result = JoinGenericReps (structure Outer=Outer and Inner=Inner)
   open Result
   structure This = struct
      fun getT ? = Pair.fst (Outer.getT ?)
      fun getS ? = Pair.fst (Outer.getS ?)
      fun getP ? = Pair.fst (Outer.getP ?)
      fun mapT ? = Outer.mapT (Pair.mapFst ?)
      fun mapS ? = Outer.mapS (Pair.mapFst ?)
      fun mapP ? = Outer.mapP (Pair.mapFst ?)
   end
end

functor LayerDepGeneric (Arg : LAYER_DEP_GENERIC_DOM) :>
   OPEN_GENERIC
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Result.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Result.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Result.p =
struct
   structure Rep = Arg.Result

   structure Inner = Rep.Inner
   structure Outer = Arg.Outer

   fun op1 mk get outer this x2y a = outer (fn x => mk (this a, x2y (get x))) a
   fun op2 mk getx gety outer this xy2z ab =
       outer (fn (x, y) => mk (this ab, xy2z (getx x, gety y))) ab
   fun m mk get outer this f b =
       outer (fn y => fn i => mk (this b i, f (get y) i)) b

   fun op0t outer this x = outer (Inner.mkT (this, x))
   fun op1t ? = op1 Inner.mkT Inner.getT ?
   fun t ? = op1 Inner.mkP Inner.getT ?
   fun r outer this lx2y l a =
       outer (fn l => fn x => Inner.mkP (this l a, lx2y l (Inner.getT x))) l a
   fun p ? = op1 Inner.mkT Inner.getP ?
   fun s ? = op1 Inner.mkT Inner.getS ?
   fun c0 outer l2s l2x = outer (Inner.mkS o Pair.map (l2s, l2x) o Sq.mk)
   fun c1 outer this cx2y c a =
       outer (fn c => fn x => Inner.mkS (this c a, cx2y c (Inner.getT x))) c a
   fun y outer x y = outer (Inner.mkY (x, y))
   fun re outer this ex a =
       outer (fn x => fn e => (this a e : Unit.t ; ex (Inner.getS x) e : Unit.t)) a

   fun iso ? = m Inner.mkT Inner.getT Outer.iso Arg.iso ?
   fun isoProduct ? = m Inner.mkP Inner.getP Outer.isoProduct Arg.isoProduct ?
   fun isoSum ? = m Inner.mkS Inner.getS Outer.isoSum Arg.isoSum ?
   fun op *` ? = op2 Inner.mkP Inner.getP Inner.getP Outer.*` Arg.*` ?
   fun T ? = t Outer.T Arg.T ?
   fun R ? = r Outer.R Arg.R ?
   fun tuple ? = p Outer.tuple Arg.tuple ?
   fun record ? = p Outer.record Arg.record ?
   fun op +` ? = op2 Inner.mkS Inner.getS Inner.getS Outer.+` Arg.+` ?
   fun C0 ? = c0 Outer.C0 Arg.C0 ?
   fun C1 ? = c1 Outer.C1 Arg.C1 ?
   fun data ? = s Outer.data Arg.data ?
   fun unit ? = op0t Outer.unit Arg.unit ?
   fun Y ? = y Outer.Y Arg.Y ?
   fun op --> ? = op2 Inner.mkT Inner.getT Inner.getT Outer.--> Arg.--> ?
   fun exn ? = op0t Outer.exn Arg.exn ?
   fun regExn ? = re Outer.regExn Arg.regExn ?
   fun array ? = op1t Outer.array Arg.array ?
   fun refc ? = op1t Outer.refc Arg.refc ?
   fun vector ? = op1t Outer.vector Arg.vector ?
   fun largeInt ? = op0t Outer.largeInt Arg.largeInt ?
   fun largeReal ? = op0t Outer.largeReal Arg.largeReal ?
   fun largeWord ? = op0t Outer.largeWord Arg.largeWord ?
   fun word8 ? = op0t Outer.word8 Arg.word8 ?
(* val word16 ? = op0t Outer.word16 Arg.word16 ? (* Word16 not provided by SML/NJ *) *)
   fun word32 ? = op0t Outer.word32 Arg.word32 ?
   fun word64 ? = op0t Outer.word64 Arg.word64 ?
   fun list ? = op1t Outer.list Arg.list ?
   fun bool ? = op0t Outer.bool Arg.bool ?
   fun char ? = op0t Outer.char Arg.char ?
   fun int ? = op0t Outer.int Arg.int ?
   fun real ? = op0t Outer.real Arg.real ?
   fun string ? = op0t Outer.string Arg.string ?
   fun word ? = op0t Outer.word Arg.word ?
end

functor LayerGeneric (Arg : LAYER_GENERIC_DOM) :>
   OPEN_GENERIC
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Result.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Result.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Result.p =
   LayerDepGeneric
     (open Arg Arg.Result.This
      fun iso b = Arg.iso (getT b)
      fun isoProduct b = Arg.isoProduct (getP b)
      fun isoSum b = Arg.isoSum (getS b)
      fun op2 geta getb this = this o Pair.map (geta, getb)
      fun op *` ? = op2 getP getP Arg.*` ?
      fun op +` ? = op2 getS getS Arg.+` ?
      fun op --> ? = op2 getT getT Arg.--> ?
      fun array a = Arg.array (getT a)
      fun vector a = Arg.vector (getT a)
      fun list a = Arg.list (getT a)
      fun refc a = Arg.refc (getT a)
      fun T a = Arg.T (getT a)
      fun R l a = Arg.R l (getT a)
      fun tuple a = Arg.tuple (getP a)
      fun record a = Arg.record (getP a)
      fun C1 c a = Arg.C1 c (getT a)
      fun data a = Arg.data (getS a)
      fun regExn a e = Arg.regExn (getS a) e)
