(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkIntInfExt (IntInf : BASIS_INT_INF) : INT_INF = struct
   structure Integer = MkIntegerExt (IntInf)
   open Integer
   open IntInf
   type bitwise = t
   type shiftable = t
end
