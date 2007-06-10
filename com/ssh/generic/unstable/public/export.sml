(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature GENERICS = GENERICS

signature GENERICS_UTIL = GENERICS_UTIL

signature CLOSED_GENERIC = CLOSED_GENERIC
signature CLOSED_GENERIC_REP = CLOSED_GENERIC_REP

signature OPEN_GENERIC = OPEN_GENERIC
signature OPEN_GENERIC_REP = OPEN_GENERIC_REP

signature GENERIC = GENERIC
signature GENERIC_EXTRA = GENERIC_EXTRA

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

structure RootGeneric : OPEN_GENERIC = RootGeneric

(** == Exported Functors == *)

functor CloseGeneric (Arg : OPEN_GENERIC) :
   CLOSED_GENERIC
      where type 'a Rep.t = ('a, Unit.t) Arg.Rep.t
      where type 'a Rep.s = ('a, Unit.t) Arg.Rep.s
      where type ('a, 'k) Rep.p = ('a, 'k, Unit.t) Arg.Rep.p =
   CloseGeneric (Arg)
(** Closes an open generic. *)

functor OpenGeneric (Arg : CLOSED_GENERIC) :
   OPEN_GENERIC
      where type ('a, 'x) Rep.t = 'a Arg.Rep.t * 'x
      where type ('a, 'x) Rep.s = 'a Arg.Rep.s * 'x
      where type ('a, 'k, 'x) Rep.p = ('a, 'k) Arg.Rep.p * 'x =
   OpenGeneric (Arg)
(** Opens a closed generic. *)

signature JOIN_GENERICS_DOM = JOIN_GENERICS_DOM

functor JoinGenerics (Arg : JOIN_GENERICS_DOM) :
   OPEN_GENERIC
      where type ('a, 'b) Rep.t =
                 ('a, ('a, 'b) Arg.Inner.Rep.t) Arg.Outer.Rep.t
      where type ('a, 'b) Rep.s =
                 ('a, ('a, 'b) Arg.Inner.Rep.s) Arg.Outer.Rep.s
      where type ('a, 'b, 'c) Rep.p =
                 ('a, 'b, ('a, 'b, 'c) Arg.Inner.Rep.p) Arg.Outer.Rep.p =
   JoinGenerics (Arg)
(**
 * Joins two extensible generic functions.  As can be read from the where
 * -constraints, the type-indices of the joined generic are compatible
 * with the type-indices of the {Outer} generic.
 *)

functor WithExtra (Arg : GENERIC) : GENERIC_EXTRA = WithExtra (Arg)
(**
 * Implements a number of frequently used type-indices for convenience.
 * As a side-effect, this functor also registers handlers for most
 * standard exceptions.  The exact set of extra type-indices is likely to
 * grow over time.
 *)

(** === Value Functors === *)

signature WITH_ARBITRARY_DOM = WITH_ARBITRARY_DOM

functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_GENERIC =
   WithArbitrary (Arg)

functor WithDummy (Arg : OPEN_GENERIC) : DUMMY_GENERIC = WithDummy (Arg)

functor WithEq (Arg : OPEN_GENERIC) : EQ_GENERIC = WithEq (Arg)

functor WithOrd (Arg : OPEN_GENERIC) : ORD_GENERIC = WithOrd (Arg)

functor WithShow (Arg : OPEN_GENERIC) : SHOW_GENERIC = WithShow (Arg)

functor WithTypeInfo (Arg : OPEN_GENERIC) : TYPE_INFO_GENERIC =
   WithTypeInfo (Arg)
