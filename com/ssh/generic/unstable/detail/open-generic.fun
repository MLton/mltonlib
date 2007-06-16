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

   fun unary arg fx = Pair.map (arg, fx)
   fun binary arg fxy x = Pair.map (arg x, fxy x)
   fun binop arg fxy = Pair.map (arg, fxy) o Pair.swizzle
   fun morph arg f (a, x) aIb = (arg a aIb, f x aIb)

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?
   fun op *` ? = binop Arg.*` ?
   fun T ? = unary Arg.T ?
   fun R ? = binary Arg.R ?
   fun tuple ? = unary Arg.tuple ?
   fun record ? = unary Arg.record ?
   fun op +` ? = binop Arg.+` ?
   fun C0 fc c = (Arg.C0 c, fc c)
   fun C1 ? = binary Arg.C1 ?
   fun data ? = unary Arg.data ?
   fun unit x = (Arg.unit, x)
   fun Y y = Tie.tuple2 (Arg.Y, y)
   fun op --> ? = binop Arg.--> ?
   fun exn x = (Arg.exn, x)
   fun regExn x2ef (a, x) = Pair.app (Arg.regExn a, x2ef x) o Sq.mk
   fun array ? = unary Arg.array ?
   fun refc ? = unary Arg.refc ?
   fun vector ? = unary Arg.vector ?
   fun largeInt x = (Arg.largeInt, x)
   fun largeReal x = (Arg.largeReal, x)
   fun largeWord x = (Arg.largeWord, x)
   fun word8 x = (Arg.word8, x)
(* fun word16 x = (Arg.word16, x) (* Word16 not provided by SML/NJ *) *)
   fun word32 x = (Arg.word32, x)
   fun word64 x = (Arg.word64, x)
   fun list ? = unary Arg.list ?
   fun bool x = (Arg.bool, x)
   fun char x = (Arg.char, x)
   fun int x = (Arg.int, x)
   fun real x = (Arg.real, x)
   fun string x = (Arg.string, x)
   fun word x = (Arg.word, x)
end
