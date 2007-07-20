(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic :> sig
   include GENERIC_EXTRA
   include ARBITRARY sharing Open.Rep = Arbitrary
   include EQ        sharing Open.Rep = Eq
   include HASH      sharing Open.Rep = Hash
   include ORD       sharing Open.Rep = Ord
   include PRETTY    sharing Open.Rep = Pretty
   include SOME      sharing Open.Rep = Some
   include TYPE_INFO sharing Open.Rep = TypeInfo
end = struct
   structure Open = RootGeneric

   structure Open = WithTypeInfo  (Open) open Open structure TypeInfo = Open

   structure Open = WithPretty    (Open) open Open
   structure Open = WithEq        (Open) open Open
   structure Open = WithOrd       (Open) open Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
   end

   structure Open = WithSome      (Open) open Open

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

   structure Arbitrary = Rep
   structure Some      = Rep
   structure Eq        = Rep
   structure Hash      = Rep
   structure Ord       = Rep
   structure Pretty    = Rep
   structure TypeInfo  = Rep

   structure Generic = struct
      structure Open = Open
      structure Closed = CloseGeneric (Open)
      open Closed
   end

   structure Extra = WithExtra (Generic) open Extra
end
