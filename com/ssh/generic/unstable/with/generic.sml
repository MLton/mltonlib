(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   structure Open : OPEN_CASES
end

functor MkGeneric (Arg : Generic) : Generic = Arg

structure Generic = struct
   structure Open = RootGeneric
end
