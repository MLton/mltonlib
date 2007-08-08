(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic :> sig
   include GENERIC_EXTRA
      where type Label.t = Generics.Label.t
      where type Con.t = Generics.Con.t
      where type Record.t = Generics.Record.t
      where type Tuple.t = Generics.Tuple.t
   include ARBITRARY     sharing Open.Rep = Arbitrary
   include DATA_REC_INFO sharing Open.Rep = DataRecInfo
   include EQ            sharing Open.Rep = Eq
   include HASH          sharing Open.Rep = Hash
   include ORD           sharing Open.Rep = Ord
   include PICKLE        sharing Open.Rep = Pickle
   include PRETTY        sharing Open.Rep = Pretty
   include SOME          sharing Open.Rep = Some
   include TYPE_INFO     sharing Open.Rep = TypeInfo
end = struct
   structure Open = RootGeneric

   (* Add generics not depending on any other generic: *)
   structure Open = WithEq          (Open) open Open structure Eq=Open
   structure Open = WithOrd         (Open) open Open
   structure Open = WithPretty      (Open) open Open
   structure Open = WithTypeInfo    (Open) open Open structure TypeInfo=Open
   structure Open = WithDataRecInfo (Open) open Open structure DataRecInfo=Open

   (* Add generics depending on other generics: *)
   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
      structure RandomGen = RanQD1Gen
   end
   structure Open = WithArbitrary   (Open) open Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
   end
   structure Open = WithHash        (Open) open Open structure Hash=Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfo = Rep
   end
   structure Open = WithSome        (Open) open Open structure Some=Open

   structure Open = struct
      open Eq Hash TypeInfo DataRecInfo Some
      structure Eq=Rep and Hash=Rep and TypeInfo=Rep and DataRecInfo=Rep
   end
   structure Open = WithPickle      (Open) open Open

   (* Make type representations equal: *)
   structure Arbitrary   = Rep
   structure Eq          = Rep
   structure Hash        = Rep
   structure Ord         = Rep
   structure Pickle      = Rep
   structure Pretty      = Rep
   structure Some        = Rep
   structure TypeInfo    = Rep
   structure DataRecInfo = Rep

   (* Close the combination for use: *)
   structure Generic = struct
      structure Open = Open
      structure Closed = CloseCases (Open)
      open Closed
   end

   (* Add extra type representation constructors: *)
   structure Extra = WithExtra (Generic) open Extra
end
