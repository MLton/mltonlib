(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature WORD = sig
   include WORD
   val fromLarge : LargeWord.word -> word
   val toLarge : word -> LargeWord.word
   val toLargeX : word -> LargeWord.word
end
