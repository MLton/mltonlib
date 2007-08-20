(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Abstract Signatures == *)

signature CLOSED_CASES = CLOSED_CASES
signature CLOSED_REP = CLOSED_REP

signature OPEN_CASES = OPEN_CASES
signature OPEN_REP = OPEN_REP

signature LAYERED_REP = LAYERED_REP

signature GENERIC = GENERIC

(** == Auxiliary Modules == *)

signature GENERICS = GENERICS
structure Generics : GENERICS = Generics

signature GENERICS_UTIL = GENERICS_UTIL
structure GenericsUtil : GENERICS_UTIL = GenericsUtil

structure RootGeneric : OPEN_CASES = RootGeneric

(** == Framework Functors == *)

functor CloseCases (Arg : OPEN_CASES) :
   CLOSED_CASES
      where type  'a      Rep.t = ('a,     Unit.t) Arg.Rep.t
      where type  'a      Rep.s = ('a,     Unit.t) Arg.Rep.s
      where type ('a, 'k) Rep.p = ('a, 'k, Unit.t) Arg.Rep.p =
   CloseCases (Arg)
(** Closes open structural cases. *)

signature LAYER_REP_DOM = LAYER_REP_DOM

functor LayerRep (Arg : LAYER_REP_DOM) :>
   LAYERED_REP
      where type  'a      Closed.t =  'a      Arg.Closed.t
      where type  'a      Closed.s =  'a      Arg.Closed.s
      where type ('a, 'k) Closed.p = ('a, 'k) Arg.Closed.p

      where type ('a,     'x) Outer.t = ('a,     'x) Arg.Outer.t
      where type ('a,     'x) Outer.s = ('a,     'x) Arg.Outer.s
      where type ('a, 'k, 'x) Outer.p = ('a, 'k, 'x) Arg.Outer.p =
   LayerRep (Arg)
(**
 * Creates a layered representation for {LayerCases} and {LayerDepCases}.
 *)

signature LAYER_CASES_DOM = LAYER_CASES_DOM

functor LayerCases (Arg : LAYER_CASES_DOM) :
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Result.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Result.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Result.p =
   LayerCases (Arg)
(**
 * Joins an outer open generic function and a closed generic function.
 *)

signature LAYER_DEP_CASES_DOM = LAYER_DEP_CASES_DOM

functor LayerDepCases (Arg : LAYER_DEP_CASES_DOM) :>
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Result.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Result.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Result.p =
   LayerDepCases (Arg)
(**
 * Joins an outer open generic function and a closed generic function that
 * depends on the outer generic.
 *)

signature GENERIC_EXTRA = GENERIC_EXTRA
functor WithExtra (Arg : GENERIC) : GENERIC_EXTRA = WithExtra (Arg)
(**
 * Implements a number of frequently used type representations for
 * convenience.  As a side-effect, this functor also registers handlers
 * for most standard exceptions.  The exact set of extra representations
 * is likely to grow over time.
 *)

(** == Auxiliary Generics == *)

signature DATA_REC_INFO = DATA_REC_INFO
signature DATA_REC_INFO_CASES = DATA_REC_INFO_CASES
functor WithDataRecInfo (Arg : OPEN_CASES) : DATA_REC_INFO_CASES =
   WithDataRecInfo (Arg)

functor WithDebug (Arg : OPEN_CASES) : OPEN_CASES = WithDebug (Arg)
(**
 * Checks dynamically that
 * - labels are unique within each record,
 * - constructors are unique within each datatype, and
 * - exception constructors are globally unique.
 *)

signature TYPE_INFO = TYPE_INFO
signature TYPE_INFO_CASES = TYPE_INFO_CASES
functor WithTypeInfo (Arg : OPEN_CASES) : TYPE_INFO_CASES = WithTypeInfo (Arg)

(** == Generics == *)

signature ARBITRARY = ARBITRARY
signature ARBITRARY_CASES = ARBITRARY_CASES
signature WITH_ARBITRARY_DOM = WITH_ARBITRARY_DOM
functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_CASES =
   WithArbitrary (Arg)

signature DYNAMIC = DYNAMIC
signature DYNAMIC_CASES = DYNAMIC_CASES
functor WithDynamic (Arg : OPEN_CASES) : DYNAMIC_CASES = WithDynamic (Arg)

signature EQ = EQ
signature EQ_CASES = EQ_CASES
functor WithEq (Arg : OPEN_CASES) : EQ_CASES = WithEq (Arg)

signature HASH = HASH
signature HASH_CASES = HASH_CASES
signature WITH_HASH_DOM = WITH_HASH_DOM
functor WithHash (Arg : WITH_HASH_DOM) : HASH_CASES = WithHash (Arg)

signature ORD = ORD
signature ORD_CASES = ORD_CASES
functor WithOrd (Arg : OPEN_CASES) : ORD_CASES = WithOrd (Arg)

signature PICKLE = PICKLE
signature PICKLE_CASES = PICKLE_CASES
signature WITH_PICKLE_DOM = WITH_PICKLE_DOM
functor WithPickle (Arg : WITH_PICKLE_DOM) : PICKLE_CASES = WithPickle (Arg)

signature PRETTY = PRETTY
signature PRETTY_CASES = PRETTY_CASES
functor WithPretty (Arg : OPEN_CASES) : PRETTY_CASES = WithPretty (Arg)

signature REDUCE = REDUCE
signature REDUCE_CASES = REDUCE_CASES
functor WithReduce (Arg : OPEN_CASES) : REDUCE_CASES = WithReduce (Arg)

signature SEQ = SEQ
signature SEQ_CASES = SEQ_CASES
functor WithSeq (Arg : OPEN_CASES) : SEQ_CASES = WithSeq (Arg)

signature SOME = SOME
signature SOME_CASES = SOME_CASES
signature WITH_SOME_DOM = WITH_SOME_DOM
functor WithSome (Arg : WITH_SOME_DOM) : SOME_CASES = WithSome (Arg)

signature TRANSFORM = TRANSFORM
signature TRANSFORM_CASES = TRANSFORM_CASES
functor WithTransform (Arg : OPEN_CASES) : TRANSFORM_CASES = WithTransform (Arg)
