(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor OpenGenericRep (Arg : CLOSED_GENERIC_REP) :
   OPEN_GENERIC_REP
      where type ('a, 'x) t = 'a Arg.t * 'x
      where type ('a, 'x) s = 'a Arg.s * 'x
      where type ('a, 'k, 'x) p = ('a, 'k) Arg.p * 'x =
struct
   val get = Pair.snd
   fun map f = Pair.map (Fn.id, f)

   type ('a, 'x) t = 'a Arg.t * 'x
   val getT = get
   val mapT = map

   type ('a, 'x) s = 'a Arg.s * 'x
   val getS = get
   val mapS = map

   type ('a, 'k, 'x) p = ('a, 'k) Arg.p * 'x
   val getP = get
   val mapP = map
end

functor OpenGeneric (Arg : CLOSED_GENERIC) :>
   OPEN_GENERIC
      where type ('a, 'x) Rep.t = 'a Arg.Rep.t * 'x
      where type ('a, 'x) Rep.s = 'a Arg.Rep.s * 'x
      where type ('a, 'k, 'x) Rep.p = ('a, 'k) Arg.Rep.p * 'x =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Rep = OpenGenericRep (Arg.Rep)

   fun op0 ? = GenericsUtil.op0 id ?
   fun op1 ? = GenericsUtil.op1 id ?
   fun op2 ? = GenericsUtil.op2 id ?
   fun morph ? = GenericsUtil.morph id ?

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?
   fun op *` ? = op2 Arg.*` ?
   fun T ? = GenericsUtil.t id Arg.T ?
   fun R ? = GenericsUtil.r id Arg.R ?
   fun tuple ? = op1 Arg.tuple ?
   fun record ? = op1 Arg.record ?
   fun op +` ? = op2 Arg.+` ?
   fun C0 ? = GenericsUtil.c0 id Arg.C0 ?
   fun C1 ? = GenericsUtil.c1 id Arg.C1 ?
   fun data ? = op1 Arg.data ?
   fun unit ? = op0 Arg.unit ?
   fun Y ? = GenericsUtil.y id Arg.Y ?
   fun op --> ? = op2 Arg.--> ?
   fun exn ? = op0 Arg.exn ?
   fun regExn ? = GenericsUtil.re id Arg.regExn ?
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
