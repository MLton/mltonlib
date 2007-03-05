(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkEquality (Core : EQUALITY_CORE) : EQUALITY = struct
   open Core
   type equality_ex = equality
   val != = not o ==
end
