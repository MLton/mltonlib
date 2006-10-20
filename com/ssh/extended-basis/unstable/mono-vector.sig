(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_VECTOR} signature.
 *)
signature MONO_VECTOR = sig
   include MONO_VECTOR

   (** == Conversions == *)

   val fromPoly : elem Vector.vector -> vector

   val toList : vector -> elem list
   val toPoly : vector -> elem Vector.vector

   (** == Isomorphisms == *)

   val listIso : (vector, elem list) iso
   val polyIso : (vector, elem Vector.vector) iso
end
