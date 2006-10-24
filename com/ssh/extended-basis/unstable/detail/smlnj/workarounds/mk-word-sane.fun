(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkWordSane (W : WORD) = struct
   open W
   val fromLarge = fromLargeWord
   val toLarge = toLargeWord
   val toLargeX = toLargeWordX
end
