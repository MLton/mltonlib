(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure BinOp :> BIN_OP = struct
   open BinOp
   fun map (b2a, a2b) = Fn.map (Sq.map b2a, a2b)
end
