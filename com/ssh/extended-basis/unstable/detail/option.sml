(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Option : OPTION = struct
   open Option
   val isNone = fn NONE   => true
                 | SOME _ => false

   fun collate cmp = fn (NONE, NONE)       => EQUAL
                      | (SOME _, NONE)     => GREATER
                      | (NONE, SOME _)     => LESS
                      | (SOME x1, SOME x2) => cmp (x1, x2) 

end
