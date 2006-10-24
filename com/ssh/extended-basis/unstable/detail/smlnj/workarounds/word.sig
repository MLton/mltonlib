(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD = sig
   include WORD
   val fromLarge : LargeWord.word -> word
   val toLarge : word -> LargeWord.word
   val toLargeX : word -> LargeWord.word
end
