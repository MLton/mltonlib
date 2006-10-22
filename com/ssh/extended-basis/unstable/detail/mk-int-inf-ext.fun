(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {INT_INF} modules.
 *)
functor MkIntInfExt (I : INT_INF) = struct
   local
      structure E = MkIntegerExt (I)
   in
      open E I
   end
end
