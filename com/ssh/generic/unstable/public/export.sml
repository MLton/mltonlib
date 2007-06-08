(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature GENERICS = GENERICS

signature GENERICS_UTIL = GENERICS_UTIL

signature GENERIC = GENERIC
signature GENERIC_INDEX = GENERIC_INDEX

signature EXT_GENERIC = EXT_GENERIC
signature EXT_GENERIC_INDEX = EXT_GENERIC_INDEX

(** === Value Signatures === *)

signature ARBITRARY = ARBITRARY
signature ARBITRARY_GENERIC = ARBITRARY_GENERIC

signature DUMMY = DUMMY
signature DUMMY_GENERIC = DUMMY_GENERIC

signature EQ = EQ
signature EQ_GENERIC = EQ_GENERIC

signature ORD = ORD
signature ORD_GENERIC = ORD_GENERIC

signature SHOW = SHOW
signature SHOW_GENERIC = SHOW_GENERIC

signature TYPE_INFO = TYPE_INFO
signature TYPE_INFO_GENERIC = TYPE_INFO_GENERIC

(** == Exported Structures == *)

structure Generics : GENERICS = Generics
structure GenericsUtil : GENERICS_UTIL = GenericsUtil

structure ExtGeneric : EXT_GENERIC = ExtGeneric

(** == Exported Functors == *)

functor GroundGeneric (Arg : EXT_GENERIC) :
   GENERIC
      where type 'a Index.t = ('a, Unit.t) Arg.Index.t
      where type 'a Index.s = ('a, Unit.t) Arg.Index.s
      where type ('a, 'k) Index.p = ('a, 'k, Unit.t) Arg.Index.p =
   GroundGeneric (Arg)
(** Grounds an extensible generic to an ordinary generic. *)

functor LiftGeneric (Arg : GENERIC) :
   EXT_GENERIC
      where type ('a, 'x) Index.t = 'a Arg.Index.t * 'x
      where type ('a, 'x) Index.s = 'a Arg.Index.s * 'x
      where type ('a, 'k, 'x) Index.p = ('a, 'k) Arg.Index.p * 'x =
   LiftGeneric (Arg)
(** Lifts an ordinary generic to an extensible generic. *)

signature JOIN_GENERICS_DOM = JOIN_GENERICS_DOM

functor JoinGenerics (Arg : JOIN_GENERICS_DOM) :
   EXT_GENERIC
      where type ('a, 'b) Index.t =
                 ('a, ('a, 'b) Arg.Inner.Index.t) Arg.Outer.Index.t
      where type ('a, 'b) Index.s =
                 ('a, ('a, 'b) Arg.Inner.Index.s) Arg.Outer.Index.s
      where type ('a, 'b, 'c) Index.p =
                 ('a, 'b, ('a, 'b, 'c) Arg.Inner.Index.p) Arg.Outer.Index.p =
   JoinGenerics (Arg)
(**
 * Joins two extensible generic functions.  As can be read from the where
 * -constraints, the type-indices of the joined generic are compatible
 * with the type-indices of the {Outer} generic.
 *)

(** === Value Functors === *)

signature WITH_ARBITRARY_DOM = WITH_ARBITRARY_DOM

functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_GENERIC =
   WithArbitrary (Arg)

functor WithDummy (Arg : EXT_GENERIC) : DUMMY_GENERIC = WithDummy (Arg)

functor WithEq (Arg : EXT_GENERIC) : EQ_GENERIC = WithEq (Arg)

functor WithOrd (Arg : EXT_GENERIC) : ORD_GENERIC = WithOrd (Arg)

functor WithShow (Arg : EXT_GENERIC) : SHOW_GENERIC = WithShow (Arg)

functor WithTypeInfo (Arg : EXT_GENERIC) : TYPE_INFO_GENERIC =
   WithTypeInfo (Arg)
