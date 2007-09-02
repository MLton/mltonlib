(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic :> sig
   include GENERIC_EXTRA
   include ARBITRARY     sharing Open.Rep = Arbitrary
   include DATA_REC_INFO sharing Open.Rep = DataRecInfo
   include EQ            sharing Open.Rep = Eq
   include HASH          sharing Open.Rep = Hash
   include ORD           sharing Open.Rep = Ord
   include PICKLE        sharing Open.Rep = Pickle
   include PRETTY        sharing Open.Rep = Pretty
   include SOME          sharing Open.Rep = Some
   include TYPE_HASH     sharing Open.Rep = TypeHash
   include TYPE_INFO     sharing Open.Rep = TypeInfo
end = struct
   structure Open = RootGeneric

   (* Add generics not depending on any other generic: *)
   structure Open = WithEq          (Open) open Open structure Eq=Open
   structure Open = WithTypeHash    (Open) open Open structure TypeHash=Open
   structure Open = WithTypeInfo    (Open) open Open structure TypeInfo=Open
   structure Open = WithDataRecInfo (Open) open Open structure DataRecInfo=Open

   (* Add generics depending on other generics: *)

   structure Open = struct
      open TypeHash TypeInfo Open
      structure TypeHash = Rep and TypeInfo = Rep
   end
   structure Open = WithHash        (Open) open Open structure Hash=Open

   structure Open = WithOrd         (Open) open Open

   structure Open = struct
      open Hash Open
      structure Hash = Rep
   end
   structure Open = WithPretty      (Open) open Open

   structure Open = struct
      open Hash TypeInfo Open
      structure Hash = Rep and TypeInfo = Rep
      structure RandomGen = RanQD1Gen
   end
   structure Open = WithArbitrary   (Open) open Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
   end
   structure Open = WithSome        (Open) open Open structure Some=Open

   structure Open = struct
      open DataRecInfo Eq Hash Some TypeHash TypeInfo Open
      structure DataRecInfo = Rep and Eq = Rep and Hash = Rep and Some = Rep
            and TypeHash = Rep and TypeInfo = Rep
   end
   structure Open = WithPickle      (Open) open Open

   (* Make type representations equal: *)
   structure Arbitrary   = Rep
   structure DataRecInfo = Rep
   structure Eq          = Rep
   structure Hash        = Rep
   structure Ord         = Rep
   structure Pickle      = Rep
   structure Pretty      = Rep
   structure Some        = Rep
   structure TypeHash    = Rep
   structure TypeInfo    = Rep

   (* Close the combination for use: *)
   structure Generic = struct
      structure Open = Open
      structure Closed = CloseCases (Open)
      open Closed
   end

   (* Add extra type representation constructors: *)
   structure Extra = WithExtra (Generic) open Extra
end
