(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {INT_INF} modules.
 *)

functor MkIntInfExt (I : INT_INF) =
   let
      structure E = MkIntegerExt (I)
   in
      struct
         open E I
      end
   end
