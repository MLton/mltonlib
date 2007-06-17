(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature THIS_GENERIC_REP = sig
   structure Rep : OPEN_GENERIC_REP
   structure Closed : CLOSED_GENERIC_REP
   val getT : ('a, 'x) Rep.t -> 'a Closed.t
   val getS : ('a, 'x) Rep.s -> 'a Closed.s
   val getP : ('a, 'k, 'x) Rep.p -> ('a, 'k) Closed.p
   val mapT : 'a Closed.t UnOp.t -> ('a, 'x) Rep.t UnOp.t
   val mapS : 'a Closed.s UnOp.t -> ('a, 'x) Rep.s UnOp.t
   val mapP : ('a, 'k) Closed.p UnOp.t -> ('a, 'k, 'x) Rep.p UnOp.t
end

signature OPENED_GENERIC_REP = sig
   include OPEN_GENERIC_REP
   structure This : THIS_GENERIC_REP
   sharing type t = This.Rep.t
   sharing type s = This.Rep.s
   sharing type p = This.Rep.p
end

functor OpenGenericRep (Arg : CLOSED_GENERIC_REP) :
   OPENED_GENERIC_REP
      where type 'a This.Closed.t = 'a Arg.t
      where type 'a This.Closed.s = 'a Arg.s
      where type ('a, 'k) This.Closed.p = ('a, 'k) Arg.p =
struct
   structure This = struct
      structure Rep = struct
         type ('a, 'x) t = 'a Arg.t * 'x
         type ('a, 'x) s = 'a Arg.s * 'x
         type ('a, 'k, 'x) p = ('a, 'k) Arg.p * 'x
         val getT = Pair.snd
         val getS = Pair.snd
         val getP = Pair.snd
         val mapT = Pair.mapSnd
         val mapS = Pair.mapSnd
         val mapP = Pair.mapSnd
      end
      structure Closed = Arg
      val getT = Pair.fst
      val getS = Pair.fst
      val getP = Pair.fst
      val mapT = Pair.mapFst
      val mapS = Pair.mapFst
      val mapP = Pair.mapFst
   end
   open This.Rep
end

signature OPENED_GENERIC = sig
   include OPEN_GENERIC
   structure This : THIS_GENERIC_REP
   sharing Rep = This.Rep
end

functor OpenGeneric (Arg : CLOSED_GENERIC) :>
   OPENED_GENERIC
      where type 'a This.Closed.t = 'a Arg.Rep.t
      where type 'a This.Closed.s = 'a Arg.Rep.s
      where type ('a, 'k) This.Closed.p = ('a, 'k) Arg.Rep.p =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Rep = OpenGenericRep (Arg.Rep)
   structure This = Rep.This

   fun op0 t x = (t, x)
   fun op1 f g = Pair.map (f, g)
   fun op2 f g = Pair.map (f, g) o Pair.swizzle
   fun morph iso' f (a, x) i = (iso' a i, f x i)
   val t = op1
   fun r lt2p lx2y = Pair.map o Pair.map (lt2p, lx2y) o Sq.mk
   fun c0 l2s l2x = Pair.map (l2s, l2x) o Sq.mk
   val c1 = r
   fun y x y = Tie.tuple2 (x, y)
   fun re ex ey (x, y) e = (ex x e : Unit.t ; ey y e : Unit.t)

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?
   fun op *` ? = op2 Arg.*` ?
   fun T ? = t Arg.T ?
   fun R ? = r Arg.R ?
   fun tuple ? = op1 Arg.tuple ?
   fun record ? = op1 Arg.record ?
   fun op +` ? = op2 Arg.+` ?
   fun C0 ? = c0 Arg.C0 ?
   fun C1 ? = c1 Arg.C1 ?
   fun data ? = op1 Arg.data ?
   fun unit ? = op0 Arg.unit ?
   fun Y ? = y Arg.Y ?
   fun op --> ? = op2 Arg.--> ?
   fun exn ? = op0 Arg.exn ?
   fun regExn ? = re Arg.regExn ?
   fun array ? = op1 Arg.array ?
   fun refc ? = op1 Arg.refc ?
   fun vector ? = op1 Arg.vector ?
   fun largeInt ? = op0 Arg.largeInt ?
   fun largeReal ? = op0 Arg.largeReal ?
   fun largeWord ? = op0 Arg.largeWord ?
   fun word8 ? = op0 Arg.word8 ?
(* fun word16 x = op0 Arg.word16 ? (* Word16 not provided by SML/NJ *) *)
   fun word32 ? = op0 Arg.word32 ?
   fun word64 ? = op0 Arg.word64 ?
   fun list ? = op1 Arg.list ?
   fun bool ? = op0 Arg.bool ?
   fun char ? = op0 Arg.char ?
   fun int ? = op0 Arg.int ?
   fun real ? = op0 Arg.real ?
   fun string ? = op0 Arg.string ?
   fun word ? = op0 Arg.word ?
end
