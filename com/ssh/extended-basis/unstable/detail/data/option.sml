(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Option : OPTION = struct
   open Option
   val isNone =
    fn NONE   => true
     | SOME _ => false
   fun collate cmp =
    fn (NONE,   NONE)   => EQUAL
     | (SOME _, NONE)   => GREATER
     | (NONE,   SOME _) => LESS
     | (SOME a, SOME b) => cmp (a, b)
   fun option (none, some) =
    fn NONE => none ()
     | SOME x => some x
   fun iso ? = Pair.map (map, map) ?
end
