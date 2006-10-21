(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {ARRAY} signature.
 *)
signature ARRAY = sig
   include ARRAY

   (** == Conversions == *)

   val fromVector : 'a vector -> 'a array

   val toList : 'a array -> 'a list
   val toVector : 'a array -> 'a vector

   (** == Isomorphisms == *)

   val isoList : ('a array, 'a list) iso
   val isoVector : ('a array, 'a vector) iso
end
