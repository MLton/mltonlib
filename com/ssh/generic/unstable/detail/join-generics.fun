(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor JoinGenerics (Arg : JOIN_GENERICS_DOM) :>
   EXT_GENERIC
      where type ('a, 'x) Index.t =
                 ('a, ('a, 'x) Arg.Inner.Index.t) Arg.Outer.Index.t
      where type ('a, 'x) Index.s =
                 ('a, ('a, 'x) Arg.Inner.Index.s) Arg.Outer.Index.s
      where type ('a, 'k, 'x) Index.p =
                 ('a, 'k, ('a, 'k, 'x) Arg.Inner.Index.p) Arg.Outer.Index.p =
struct
   open Arg

   structure Index : EXT_GENERIC_INDEX = struct
      type ('a, 'x) t = ('a, ('a, 'x) Inner.Index.t) Outer.Index.t
      fun getT ? = Inner.Index.getT (Outer.Index.getT ?)
      fun mapT ? = Outer.Index.mapT (Inner.Index.mapT ?)

      type ('a, 'x) s = ('a, ('a, 'x) Inner.Index.s) Outer.Index.s
      fun getS ? = Inner.Index.getS (Outer.Index.getS ?)
      fun mapS ? = Outer.Index.mapS (Inner.Index.mapS ?)

      type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k, 'x) Inner.Index.p) Outer.Index.p
      fun getP ? = Inner.Index.getP (Outer.Index.getP ?)
      fun mapP ? = Outer.Index.mapP (Inner.Index.mapP ?)
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
