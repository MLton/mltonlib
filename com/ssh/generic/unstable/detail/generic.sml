(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic : sig
   structure Open : OPEN_GENERIC

   include CLOSED_GENERIC_WITH_CONVENIENCE
      where type 'a Rep.t = ('a, Unit.t) Open.Rep.t
      where type 'a Rep.s = ('a, Unit.t) Open.Rep.s
      where type ('a, 'k) Rep.p = ('a, 'k, Unit.t) Open.Rep.p

   include ARBITRARY sharing Open.Rep = Arbitrary
   include DUMMY     sharing Open.Rep = Dummy
   include EQ        sharing Open.Rep = Eq
   include ORD       sharing Open.Rep = Ord
   include SHOW      sharing Open.Rep = Show
   include TYPE_INFO sharing Open.Rep = TypeInfo
end = struct
   structure Open = RootGeneric

   structure Open = WithShow      (Open) open Open
   structure Open = WithTypeInfo  (Open) open Open structure TypeInfo = Open
   structure Open = WithEq        (Open) open Open
   structure Open = WithOrd       (Open) open Open
   structure Open = WithDummy     (Open) open Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
      structure RandomGen = RanQD1Gen
   end

   structure Open = WithArbitrary (Open) open Open

   structure Arbitrary = Open.Rep
   structure Dummy     = Open.Rep
   structure Eq        = Open.Rep
   structure Ord       = Open.Rep
   structure Show      = Open.Rep
   structure TypeInfo  = Open.Rep

   structure Closed = WithConvenience (CloseGeneric (Open)) open Closed
end
