(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkWordSane (W : WORD) = struct
   open W
   val fromLarge = fromLargeWord
   val toLarge = toLargeWord
   val toLargeX = toLargeWordX
end
