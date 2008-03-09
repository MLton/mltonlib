(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Abstract Signatures == *)

signature CLOSED_CASES = CLOSED_CASES
signature CLOSED_REP = CLOSED_REP

signature OPEN_CASES = OPEN_CASES
signature OPEN_REP = OPEN_REP

signature CASES = CASES
signature REP = REP

signature LAYERED_REP = LAYERED_REP

signature GENERIC = GENERIC

(** == Auxiliary Modules == *)

signature GENERICS = GENERICS
structure Generics : GENERICS = Generics

signature GENERICS_UTIL = GENERICS_UTIL
structure GenericsUtil : GENERICS_UTIL = GenericsUtil

signature TY = TY
structure Ty : TY = Ty

structure RootGeneric : CASES = RootGeneric

(** == Framework Functors == *)

signature LAYER_REP_COD = LAYER_REP_COD

signature LAYER_REP_DOM = LAYER_REP_DOM
functor LayerRep (Arg : LAYER_REP_DOM) :>
   LAYER_REP_COD
      where type  'a      This.t =  'a      Arg.t
      where type  'a      This.s =  'a      Arg.s
      where type ('a, 'k) This.p = ('a, 'k) Arg.p

      where type ('a,     'x) Outer.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Outer.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Outer.p = ('a, 'k, 'x) Arg.Open.Rep.p =
   LayerRep (Arg)
(**
 * Creates a layered representation for {LayerCases} and {LayerDepCases}.
 *)

signature LAYER_REP_DOM' = LAYER_REP_DOM'
functor LayerRep' (Arg : LAYER_REP_DOM') :>
   LAYER_REP_COD
      where type  'a      This.t = 'a Arg.t
      where type  'a      This.s = 'a Arg.t
      where type ('a, 'k) This.p = 'a Arg.t

      where type ('a,     'x) Outer.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Outer.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Outer.p = ('a, 'k, 'x) Arg.Open.Rep.p =
   LayerRep' (Arg)
(**
 * Creates a layered representation for {LayerCases} and {LayerDepCases}.
 *)

signature LAYER_CASES_DOM = LAYER_CASES_DOM
functor LayerCases (Arg : LAYER_CASES_DOM) :>
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.p =
   LayerCases (Arg)
(**
 * Joins an outer open generic function and a closed generic function.
 *)

signature LAYER_DEP_CASES_DOM = LAYER_DEP_CASES_DOM
functor LayerDepCases (Arg : LAYER_DEP_CASES_DOM) :>
   OPEN_CASES
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.p =
   LayerDepCases (Arg)
(**
 * Joins an outer open generic function and a closed generic function that
 * depends on the outer generic.
 *)

(** === Closing Generics === *)

functor CloseCases (Arg : CASES) :>
   GENERIC
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Arg.Open.Rep.p =
   CloseCases (Arg)
(** Closes open structural cases. *)

signature GENERIC_EXTRA = GENERIC_EXTRA
functor WithExtra (Arg : GENERIC) : GENERIC_EXTRA = WithExtra (Arg)
(**
 * Implements a number of frequently used type representations for
 * convenience.  The exact set of extra representations is likely to grow
 * over time.
 *)

functor ClosePrettyWithExtra (Arg : PRETTY_CASES) : GENERIC_EXTRA =
   ClosePrettyWithExtra (Arg)
(**
 * Convenience for the common case of closing a collection of generics
 * including {Pretty} with extra type representations.
 *)

functor RegBasisExns (Arg : CLOSED_CASES) = RegBasisExns (Arg)
(** Registers handlers for most standard exceptions as a side-effect. *)

(** == Auxiliary Generics == *)

signature DATA_REC_INFO = DATA_REC_INFO
      and DATA_REC_INFO_CASES = DATA_REC_INFO_CASES
      and WITH_DATA_REC_INFO_DOM = WITH_DATA_REC_INFO_DOM
functor WithDataRecInfo (Arg : WITH_DATA_REC_INFO_DOM) : DATA_REC_INFO_CASES =
   WithDataRecInfo (Arg)

functor WithDebug (Arg : CASES) : OPEN_CASES = WithDebug (Arg)
(**
 * Checks dynamically that
 * - labels are unique within each record,
 * - constructors are unique within each datatype, and
 * - exception constructors are globally unique.
 *)

signature TYPE_EXP = TYPE_EXP and TYPE_EXP_CASES = TYPE_EXP_CASES
      and WITH_TYPE_EXP_DOM = WITH_TYPE_EXP_DOM
functor WithTypeExp (Arg : WITH_TYPE_EXP_DOM) : TYPE_EXP_CASES =
   WithTypeExp (Arg)

signature TYPE_INFO = TYPE_INFO and TYPE_INFO_CASES = TYPE_INFO_CASES
      and WITH_TYPE_INFO_DOM = WITH_TYPE_INFO_DOM
functor WithTypeInfo (Arg : WITH_TYPE_INFO_DOM) : TYPE_INFO_CASES =
   WithTypeInfo (Arg)

(** == Generics ==
 *
 * Although it isn't directly apparent from the "functor signatures" of
 * the generics, they are actually sealed via the layering functors.
 *)

signature ARBITRARY = ARBITRARY and ARBITRARY_CASES = ARBITRARY_CASES
      and WITH_ARBITRARY_DOM = WITH_ARBITRARY_DOM
functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_CASES =
   WithArbitrary (Arg)

signature DYNAMIC = DYNAMIC and DYNAMIC_CASES = DYNAMIC_CASES
      and WITH_DYNAMIC_DOM = WITH_DYNAMIC_DOM
functor WithDynamic (Arg : WITH_DYNAMIC_DOM) : DYNAMIC_CASES = WithDynamic (Arg)

signature ENUM = ENUM and ENUM_CASES = ENUM_CASES
      and WITH_ENUM_DOM = WITH_ENUM_DOM
functor WithEnum (Arg : WITH_ENUM_DOM) : ENUM_CASES = WithEnum (Arg)

signature EQ = EQ and EQ_CASES = EQ_CASES and WITH_EQ_DOM = WITH_EQ_DOM
functor WithEq (Arg : WITH_EQ_DOM) : EQ_CASES = WithEq (Arg)

structure FmapAux = FmapAux
signature FMAP = FMAP and FMAP_CASES = FMAP_CASES
      and WITH_FMAP_DOM = WITH_FMAP_DOM
functor WithFmap (Arg : WITH_FMAP_DOM) : FMAP_CASES = WithFmap (Arg)
signature MK_FMAP_DOM = MK_FMAP_DOM
functor MkFmap (Arg : MK_FMAP_DOM) : sig
   val map : ('a -> 'b) -> 'a Arg.t -> 'b Arg.t
end = MkFmap (Arg)

signature HASH = HASH and HASH_CASES = HASH_CASES
      and WITH_HASH_DOM = WITH_HASH_DOM
functor WithHash (Arg : WITH_HASH_DOM) : HASH_CASES = WithHash (Arg)

signature ORD = ORD and ORD_CASES = ORD_CASES and WITH_ORD_DOM = WITH_ORD_DOM
functor WithOrd (Arg : WITH_ORD_DOM) : ORD_CASES = WithOrd (Arg)

signature PICKLE = PICKLE and PICKLE_CASES = PICKLE_CASES
      and WITH_PICKLE_DOM = WITH_PICKLE_DOM
functor WithPickle (Arg : WITH_PICKLE_DOM) : PICKLE_CASES = WithPickle (Arg)

signature PRETTY = PRETTY and PRETTY_CASES = PRETTY_CASES
      and WITH_PRETTY_DOM = WITH_PRETTY_DOM
functor WithPretty (Arg : WITH_PRETTY_DOM) : PRETTY_CASES = WithPretty (Arg)

signature READ = READ and READ_CASES = READ_CASES
      and WITH_READ_DOM = WITH_READ_DOM
functor WithRead (Arg : WITH_READ_DOM) : READ_CASES = WithRead (Arg)

signature REDUCE = REDUCE and REDUCE_CASES = REDUCE_CASES
      and WITH_REDUCE_DOM = WITH_REDUCE_DOM
functor WithReduce (Arg : WITH_REDUCE_DOM) : REDUCE_CASES = WithReduce (Arg)

signature SEQ = SEQ and SEQ_CASES = SEQ_CASES and WITH_SEQ_DOM = WITH_SEQ_DOM
functor WithSeq (Arg : WITH_SEQ_DOM) : SEQ_CASES = WithSeq (Arg)

signature SHRINK = SHRINK and SHRINK_CASES = SHRINK_CASES
      and WITH_SHRINK_DOM = WITH_SHRINK_DOM
functor WithShrink (Arg : WITH_SHRINK_DOM) : SHRINK_CASES = WithShrink (Arg)

signature SIZE = SIZE and SIZE_CASES = SIZE_CASES
      and WITH_SIZE_DOM = WITH_SIZE_DOM
functor WithSize (Arg : WITH_SIZE_DOM) : SIZE_CASES = WithSize (Arg)

signature SOME = SOME and SOME_CASES = SOME_CASES
      and WITH_SOME_DOM = WITH_SOME_DOM
functor WithSome (Arg : WITH_SOME_DOM) : SOME_CASES = WithSome (Arg)

signature TRANSFORM = TRANSFORM and TRANSFORM_CASES = TRANSFORM_CASES
      and WITH_TRANSFORM_DOM = WITH_TRANSFORM_DOM
functor WithTransform (Arg : WITH_TRANSFORM_DOM) : TRANSFORM_CASES =
   WithTransform (Arg)

signature TYPE_HASH = TYPE_HASH and TYPE_HASH_CASES = TYPE_HASH_CASES
      and WITH_TYPE_HASH_DOM = WITH_TYPE_HASH_DOM
functor WithTypeHash (Arg : WITH_TYPE_HASH_DOM) : TYPE_HASH_CASES =
   WithTypeHash (Arg)

signature UNIPLATE = UNIPLATE and UNIPLATE_CASES = UNIPLATE_CASES
      and WITH_UNIPLATE_DOM = WITH_UNIPLATE_DOM
functor WithUniplate (Arg : WITH_UNIPLATE_DOM) : UNIPLATE_CASES =
   WithUniplate (Arg)
