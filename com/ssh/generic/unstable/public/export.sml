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

signature JOIN_GENERICS_DOM = JOIN_GENERICS_DOM

(** == Exported Structures == *)

structure Generic : EXT_GENERIC = Generic
structure Generics : GENERICS = Generics
structure GenericsUtil : GENERICS_UTIL = GenericsUtil

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
