(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure BinFn :> BIN_FN = struct
   open BinFn
   fun map (f, g) = Fn.map (Sq.map f, g)
end
