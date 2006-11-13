(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Bool : BOOL} structure.
 *)
structure Bool : BOOL = struct
   open Bool
   type t = bool
   val equal = op = : bool * bool -> bool
   val compare = fn (false,  true) => LESS
                  | (true,  false) => GREATER
                  | (_,         _) => EQUAL
   fun isFalse b = b = false
   fun isTrue b = b = true
end
