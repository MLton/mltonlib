(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * An implementation of the BIT_FLAGS signature of the Basis Library.
 *)
structure BitFlags : BIT_FLAGS = struct
   open SysWord
   type flags = t
   val toWord = id
   val fromWord = id
   val (none, all) = bounds
   val flags = foldl op orb none
   val intersect = foldl op andb all
   fun clear (f1, f2) = notb f1 andb f2
   fun allSet (f1, f2) = f1 = f1 andb f2
   fun anySet (f1, f2) = none <> f1 andb f2
end
