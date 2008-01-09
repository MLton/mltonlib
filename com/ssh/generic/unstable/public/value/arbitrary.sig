(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for generating random values of any
 * type.  The design is inspired by the
 * [http://www.cs.chalmers.se/~rjmh/QuickCheck/ QuickCheck] library by
 * Koen Claessen and John Hughes.
 *)
signature ARBITRARY = sig
   structure ArbitraryRep : OPEN_REP

   structure RandomGen : RANDOM_GEN
   (** The underlying random value generator. *)

   val arbitrary : ('a, 'x) ArbitraryRep.t -> 'a RandomGen.t
   (** Extracts the random value generator. *)

   val withGen : 'a RandomGen.t -> ('a, 'x) ArbitraryRep.t UnOp.t
   (** Functionally updates the random value generator. *)
end

signature ARBITRARY_CASES = sig
   include CASES ARBITRARY
   sharing Open.Rep = ArbitraryRep
end

signature WITH_ARBITRARY_DOM = sig
   include CASES HASH TYPE_INFO
   sharing Open.Rep = HashRep = TypeInfoRep
   structure RandomGen : RANDOM_GEN (* = RanQD1Gen *)
end
