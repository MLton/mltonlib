(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkCStringable (Core : CSTRINGABLE_CORE) : CSTRINGABLE = struct
   open Core
   type cstringable_ex = cstringable
   val (toCString, fromCString) = embCString
end
