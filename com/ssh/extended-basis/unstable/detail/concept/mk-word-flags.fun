(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkWordFlags (Arg : WORD) : FLAGS = struct
   open Arg
   type flags = t
   type flags_word = t
   val toWord = Fn.id
   val fromWord = Fn.id
   val isoWord = Iso.id
   val (none, all) = bounds
   val flags = foldl orb none
   val intersect = foldl andb all
   fun clear (f1, f2) = andb (notb f1, f2)
   fun allSet (f1, f2) = f1 = andb (f1, f2)
   fun anySet (f1, f2) = none <> andb (f1, f2)
end
