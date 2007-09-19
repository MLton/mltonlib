(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic :> sig
   include GENERIC_EXTRA
   include ARBITRARY     sharing Open.Rep = ArbitraryRep
   include DATA_REC_INFO sharing Open.Rep = DataRecInfoRep
   include EQ            sharing Open.Rep = EqRep
   include HASH          sharing Open.Rep = HashRep
   include ORD           sharing Open.Rep = OrdRep
   include PICKLE        sharing Open.Rep = PickleRep
   include PRETTY        sharing Open.Rep = PrettyRep
   include SOME          sharing Open.Rep = SomeRep
   include TYPE_HASH     sharing Open.Rep = TypeHashRep
   include TYPE_INFO     sharing Open.Rep = TypeInfoRep
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Open = RootGeneric

   (* Add generics not depending on any other generic: *)
   structure Open = WithEq          (Open) open Open structure Eq=Open
   structure Open = WithTypeHash    (Open) open Open structure TypeHash=Open
   structure Open = WithTypeInfo    (Open) open Open structure TypeInfo=Open
   structure Open = WithDataRecInfo (Open) open Open structure DataRecInfo=Open

   (* Add generics depending on other generics: *)

   structure Open = struct
      open TypeHash TypeInfo Open
      structure TypeHashRep = Rep and TypeInfoRep = Rep
   end
   structure Open = WithHash        (Open) open Open structure Hash=Open

   structure Open = WithOrd         (Open) open Open

   structure Open = struct
      open Hash Open
      structure HashRep = Rep
   end
   structure Open = WithPretty      (Open) open Open

   structure Open = struct
      open Hash TypeInfo Open
      structure HashRep = Rep and TypeInfoRep = Rep
      structure RandomGen = RanQD1Gen
   end
   structure Open = WithArbitrary   (Open) open Open

   structure Open = struct
      open TypeInfo Open
      structure TypeInfoRep = Rep
   end
   structure Open = WithSome        (Open) open Open structure Some=Open

   structure Open = struct
      open DataRecInfo Eq Hash Some TypeHash TypeInfo Open
      structure DataRecInfoRep = Rep and EqRep = Rep and HashRep = Rep
            and SomeRep = Rep and TypeHashRep = Rep and TypeInfoRep = Rep
   end
   structure Open = WithPickle      (Open) open Open

   (* Make type representations equal: *)
   structure ArbitraryRep   = Rep
   structure DataRecInfoRep = Rep
   structure EqRep          = Rep
   structure HashRep        = Rep
   structure OrdRep         = Rep
   structure PickleRep      = Rep
   structure PrettyRep      = Rep
   structure SomeRep        = Rep
   structure TypeHashRep    = Rep
   structure TypeInfoRep    = Rep

   (* Close the combination for use: *)
   structure Generic = struct
      structure Open = Open
      structure Closed = CloseCases (Open)
      open Closed
   end

   (* Add extra type representation constructors: *)
   structure Extra = WithExtra (Generic) open Extra

   (* Pretty print products in infix: *)
   local
      val et = C "&"
   in
      fun op &` ab =
          iso (data (Pretty.infixL 0 et ab
                     (C1 et (tuple2 ab))))
              (fn op & ? => ?, op &)
   end
end
