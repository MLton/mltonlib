(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor JoinGenerics (Arg : JOIN_GENERICS_DOM) :>
   OPEN_GENERIC
      where type ('a, 'x) Rep.t =
                 ('a, ('a, 'x) Arg.Inner.Rep.t) Arg.Outer.Rep.t
      where type ('a, 'x) Rep.s =
                 ('a, ('a, 'x) Arg.Inner.Rep.s) Arg.Outer.Rep.s
      where type ('a, 'k, 'x) Rep.p =
                 ('a, 'k, ('a, 'k, 'x) Arg.Inner.Rep.p) Arg.Outer.Rep.p =
struct
   open Arg

   structure Rep : OPEN_GENERIC_REP = struct
      type ('a, 'x) t = ('a, ('a, 'x) Inner.Rep.t) Outer.Rep.t
      fun getT ? = Inner.Rep.getT (Outer.Rep.getT ?)
      fun mapT ? = Outer.Rep.mapT (Inner.Rep.mapT ?)

      type ('a, 'x) s = ('a, ('a, 'x) Inner.Rep.s) Outer.Rep.s
      fun getS ? = Inner.Rep.getS (Outer.Rep.getS ?)
      fun mapS ? = Outer.Rep.mapS (Inner.Rep.mapS ?)

      type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k, 'x) Inner.Rep.p) Outer.Rep.p
      fun getP ? = Inner.Rep.getP (Outer.Rep.getP ?)
      fun mapP ? = Outer.Rep.mapP (Inner.Rep.mapP ?)
   end

   fun iso ? = Outer.iso (Inner.iso ?)
   fun isoProduct ? = Outer.isoProduct (Inner.isoProduct ?)
   fun isoSum ? = Outer.isoSum (Inner.isoSum ?)
   fun op *` ? = Outer.*` (Inner.*` ?)
   fun T ? = Outer.T (Inner.T ?)
   fun R ? = Outer.R (Inner.R ?)
   fun tuple ? = Outer.tuple (Inner.tuple ?)
   fun record ? = Outer.record (Inner.record ?)
   fun op +` ? = Outer.+` (Inner.+` ?)
   fun C0 ? = Outer.C0 (Inner.C0 ?)
   fun C1 ? = Outer.C1 (Inner.C1 ?)
   fun data ? = Outer.data (Inner.data ?)
   fun unit ? = Outer.unit (Inner.unit ?)
   fun Y ? = Outer.Y (Inner.Y ?)
   fun op --> ? = Outer.--> (Inner.--> ?)
   fun exn ? = Outer.exn (Inner.exn ?)
   fun regExn ? = Outer.regExn (Inner.regExn ?)
   fun array ? = Outer.array (Inner.array ?)
   fun refc ? = Outer.refc (Inner.refc ?)
   fun vector ? = Outer.vector (Inner.vector ?)
   fun largeInt ? = Outer.largeInt (Inner.largeInt ?)
   fun largeReal ? = Outer.largeReal (Inner.largeReal ?)
   fun largeWord ? = Outer.largeWord (Inner.largeWord ?)
   fun word8 ? = Outer.word8 (Inner.word8 ?)
(* fun word16 ? = Outer.word16 (Inner.word16 ?)
   (* Word16 not provided by SML/NJ *) *)
   fun word32 ? = Outer.word32 (Inner.word32 ?)
   fun word64 ? = Outer.word64 (Inner.word64 ?)
   fun list ? = Outer.list (Inner.list ?)
   fun bool ? = Outer.bool (Inner.bool ?)
   fun char ? = Outer.char (Inner.char ?)
   fun int ? = Outer.int (Inner.int ?)
   fun real ? = Outer.real (Inner.real ?)
   fun string ? = Outer.string (Inner.string ?)
   fun word ? = Outer.word (Inner.word ?)
end
