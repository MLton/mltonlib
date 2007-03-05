(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkStringable (Core : STRINGABLE_CORE) : STRINGABLE = struct
   open Core
   type stringable_ex = stringable
   val (toString, fromString) = embString
end
