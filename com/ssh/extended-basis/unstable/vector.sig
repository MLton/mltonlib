(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {VECTOR} signature.
 *)
signature VECTOR = sig
   include VECTOR

   (** == Conversions == *)

   val toList : 'a vector -> 'a list

   (** == Isomorphisms == *)

   val isoList : ('a vector, 'a list) iso
end
