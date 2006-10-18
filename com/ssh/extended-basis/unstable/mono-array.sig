(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_ARRAY} signature.
 *)

signature MONO_ARRAY =
   sig
      include MONO_ARRAY
      val list : (array, elem list) iso
      val toList : array -> elem list
   end
