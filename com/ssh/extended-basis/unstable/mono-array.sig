(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_ARRAY} signature.
 *)
signature MONO_ARRAY = sig
   include MONO_ARRAY

   (** == Conversions == *)

   val fromPoly : elem Array.array -> array
   val fromVector : vector -> array

   val toList : array -> elem list
   val toPoly : array -> elem Array.array
   val toVector : array -> vector

   (** == Isomorphisms == *)

   val listIso : (array, elem list) iso
   val polyIso : (array, elem Array.array) iso
   val vectorIso : (array, vector) iso
end
