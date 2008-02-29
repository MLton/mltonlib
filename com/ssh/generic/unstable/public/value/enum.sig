(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic value enumeration.
 *
 * The main application of enumeration is testing.
 *)
signature ENUM = sig
   structure EnumRep : OPEN_REP

   structure Enum : sig
      type 'a t
      (** Type of enumeration streams. *)

      val get : ('a, 'a t) Reader.t
      (**
       * Reader for enumeration streams.
       *
       * Enumeration streams are not memoized.  Each time {Enum.get} is
       * called, a new value is created and all mutable substructures
       * generated from an enumeration will be distinct.
       *)
   end

   val enum : ('a, 'x) EnumRep.t -> 'a Enum.t
   (**
    * Returns a stream that enumerates through finite, acyclic values of
    * the type.
    *)
end

signature ENUM_CASES = sig
   include CASES ENUM
   sharing Open.Rep = EnumRep
end

signature WITH_ENUM_DOM = TYPE_INFO_CASES
