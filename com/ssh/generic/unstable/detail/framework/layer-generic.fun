(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor LayerRep (Arg : LAYER_REP_DOM) :>
   LAYER_REP_COD
      where type  'a      This.t =  'a      Arg.t
      where type  'a      This.s =  'a      Arg.s
      where type ('a, 'k) This.p = ('a, 'k) Arg.p

      where type ('a,     'x) Outer.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Outer.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Outer.p = ('a, 'k, 'x) Arg.Open.Rep.p =
struct
   structure Outer = Arg.Open.Rep
   structure Rep = Arg
   structure Inner = struct
      type ('a,     'x) t =  'a      Rep.t * 'x
      type ('a,     'x) s =  'a      Rep.s * 'x
      type ('a, 'k, 'x) p = ('a, 'k) Rep.p * 'x
      val getT = Pair.snd
      val getS = Pair.snd
      val getP = Pair.snd
      val mapT = Pair.mapSnd
      val mapS = Pair.mapSnd
      val mapP = Pair.mapSnd
   end
   type ('a,     'x) t = ('a,     ('a,     'x) Inner.t) Outer.t
   type ('a,     'x) s = ('a,     ('a,     'x) Inner.s) Outer.s
   type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k, 'x) Inner.p) Outer.p
   fun getT ? = Inner.getT (Outer.getT ?)
   fun getS ? = Inner.getS (Outer.getS ?)
   fun getP ? = Inner.getP (Outer.getP ?)
   fun mapT ? = Outer.mapT (Inner.mapT ?)
   fun mapS ? = Outer.mapS (Inner.mapS ?)
   fun mapP ? = Outer.mapP (Inner.mapP ?)
   structure This = struct
      open Rep
      fun getT ? = Pair.fst (Outer.getT ?)
      fun getS ? = Pair.fst (Outer.getS ?)
      fun getP ? = Pair.fst (Outer.getP ?)
      fun mapT ? = Outer.mapT (Pair.mapFst ?)
      fun mapS ? = Outer.mapS (Pair.mapFst ?)
      fun mapP ? = Outer.mapP (Pair.mapFst ?)
      val mkT = Fn.id
      val mkS = Fn.id
      val mkP = Fn.id
      val mkY = Tie.tuple2
   end
end

functor LayerRep' (Arg : LAYER_REP_DOM') =
   LayerRep (open Arg type 'a s = 'a t type ('a, 'k) p = 'a t)

functor LayerDepCases (Arg : LAYER_DEP_CASES_DOM) :>
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.p =
struct
   open Arg
   structure Rep = Arg

   fun op1 mk get outer this x2y a = outer (fn x => mk (this a, x2y (get x))) a
   fun op2 mk getx gety outer this xy2z ab =
       outer (fn (x, y) => mk (this ab, xy2z (getx x, gety y))) ab
   fun m mk get outer this f b =
       outer (fn y => fn i => mk (this b i, f (get y) i)) b

   fun op0t outer this x = outer (This.mkT (this, x))
   fun op1t ? = op1 This.mkT Inner.getT ?
   fun t ? = op1 This.mkP Inner.getT ?
   fun r outer this lx2y l a =
       outer (fn l => fn x => This.mkP (this l a, lx2y l (Inner.getT x))) l a
   fun p ? = op1 This.mkT Inner.getP ?
   fun s ? = op1 This.mkT Inner.getS ?
   fun c0 outer l2s l2x = outer (This.mkS o Pair.map (l2s, l2x) o Sq.mk)
   fun c1 outer this cx2y c a =
       outer (fn c => fn x => This.mkS (this c a, cx2y c (Inner.getT x))) c a
   fun y outer x y = outer (This.mkY (x, y))
   fun re0 outer this ex =
       outer (fn c => fn e => (this c e : Unit.t ; ex c e : Unit.t))
   fun re1 outer this ex c a =
       outer (fn c => fn x => fn e =>
                 (this c a e : Unit.t ; ex c (Inner.getT x) e : Unit.t)) c a

   fun iso ? = m This.mkT Inner.getT Open.iso Arg.iso ?
   fun isoProduct ? = m This.mkP Inner.getP Open.isoProduct Arg.isoProduct ?
   fun isoSum ? = m This.mkS Inner.getS Open.isoSum Arg.isoSum ?
   fun op *` ? = op2 This.mkP Inner.getP Inner.getP Open.*` Arg.*` ?
   fun T ? = t Open.T Arg.T ?
   fun R ? = r Open.R Arg.R ?
   fun tuple ? = p Open.tuple Arg.tuple ?
   fun record ? = p Open.record Arg.record ?
   fun op +` ? = op2 This.mkS Inner.getS Inner.getS Open.+` Arg.+` ?
   fun C0 ? = c0 Open.C0 Arg.C0 ?
   fun C1 ? = c1 Open.C1 Arg.C1 ?
   fun data ? = s Open.data Arg.data ?
   fun unit ? = op0t Open.unit Arg.unit ?
   fun Y ? = y Open.Y Arg.Y ?
   fun op --> ? = op2 This.mkT Inner.getT Inner.getT Open.--> Arg.--> ?
   fun exn ? = op0t Open.exn Arg.exn ?
   fun regExn0 ? = re0 Open.regExn0 Arg.regExn0 ?
   fun regExn1 ? = re1 Open.regExn1 Arg.regExn1 ?
   fun array ? = op1t Open.array Arg.array ?
   fun refc ? = op1t Open.refc Arg.refc ?
   fun vector ? = op1t Open.vector Arg.vector ?
   fun fixedInt ? = op0t Open.fixedInt Arg.fixedInt ?
   fun largeInt ? = op0t Open.largeInt Arg.largeInt ?
   fun largeReal ? = op0t Open.largeReal Arg.largeReal ?
   fun largeWord ? = op0t Open.largeWord Arg.largeWord ?
   fun word8 ? = op0t Open.word8 Arg.word8 ?
   fun word32 ? = op0t Open.word32 Arg.word32 ?
(*
   fun word64 ? = op0t Open.word64 Arg.word64 ?
*)
   fun list ? = op1t Open.list Arg.list ?
   fun bool ? = op0t Open.bool Arg.bool ?
   fun char ? = op0t Open.char Arg.char ?
   fun int ? = op0t Open.int Arg.int ?
   fun real ? = op0t Open.real Arg.real ?
   fun string ? = op0t Open.string Arg.string ?
   fun word ? = op0t Open.word Arg.word ?

   fun hole ? = Open.hole (This.mkT (Arg.hole (), ?))
end

functor LayerCases (Arg : LAYER_CASES_DOM) :>
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.p =
   LayerDepCases
     (open Arg
      local
         open Arg.This
      in
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
         fun regExn1 c = Arg.regExn1 c o getT
      end)
