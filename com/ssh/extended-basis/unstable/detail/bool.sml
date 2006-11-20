(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Bool : BOOL = struct
   open Bool
   val equal = op = : t BinOp.t
   val compare = fn (false,  true) => LESS
                  | (true,  false) => GREATER
                  | (_,         _) => EQUAL
   fun isFalse b = b = false
   fun isTrue b = b = true
end
