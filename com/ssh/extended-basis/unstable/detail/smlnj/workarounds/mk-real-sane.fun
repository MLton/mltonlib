(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkRealSane (R : REAL) = struct
   open R
   val fromDecimal' = fromDecimal
   fun fromDecimal d = SOME (fromDecimal' d) handle _ => NONE
end
