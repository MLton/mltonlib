(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkIntInfExt (I : BASIS_INT_INF) : INT_INF = struct
   local
      structure E = MkIntegerExt (I)
   in
      open E I
   end
end
