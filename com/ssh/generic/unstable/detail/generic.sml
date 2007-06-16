(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic : sig
   include GENERIC_EXTRA
   include ARBITRARY sharing Open.Rep = Arbitrary
   include DUMMY     sharing Open.Rep = Dummy
   include EQ        sharing Open.Rep = Eq
   include HASH      sharing Open.Rep = Hash
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

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
   end

   structure Open = WithHash      (Open) open Open

   structure Arbitrary = Open.Rep
   structure Dummy     = Open.Rep
   structure Eq        = Open.Rep
   structure Hash      = Open.Rep
   structure Ord       = Open.Rep
   structure Show      = Open.Rep
   structure TypeInfo  = Open.Rep

   structure Generic = struct
      structure Open = Open
      structure Closed = CloseGeneric (Open)
      open Closed
   end

   structure Extra = WithExtra (Generic) open Extra
end
