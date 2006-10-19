(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_VECTOR} signature.
 *)

signature MONO_VECTOR =
   sig
      include MONO_VECTOR
      val toList : vector -> elem list
      val listIso : (vector, elem list) iso
      val toPoly : vector -> elem Vector.vector
      val fromPoly : elem Vector.vector -> vector
      val polyIso : (vector, elem Vector.vector) iso
   end
