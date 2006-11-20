(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BinOp :> BIN_OP = struct
   open BinOp
   fun map (b2a, a2b) = Fn.map (Sq.map b2a, a2b)
end
