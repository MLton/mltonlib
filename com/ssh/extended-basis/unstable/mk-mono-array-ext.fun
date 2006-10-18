(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {MONO_ARRAY} modules.
 *)

functor MkMonoArrayExt (M : MONO_ARRAY) =
   struct
      open M
      fun toList v = foldr op :: [] v
      val list = (toList, fromList)
   end
