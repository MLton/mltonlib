(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature BIT_FLAGS = sig

   type t
   (**
    * A set of flags.
    *)

   val all: t
   val difference: t * t -> t
   val doIntersect: t * t -> Bool.t
   val fromWord: SysWord.t -> t
   val intersect: t List.t -> t
   val isSubset: t * t -> Bool.t
   val toWord: t -> SysWord.t
   val union: t List.t -> t

end
