(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkOrdered (Core : ORDERED_CORE) : ORDERED = struct
   open Core
   type ordered_ex = ordered
   type equality = ordered
   type equality_ex = ordered
   val {<, <=, >, >=, ==, !=} = Cmp.mkRelOps compare
   val max = Cmp.max compare
   val min = Cmp.min compare
   fun inRange (lo, hi) =
       if hi < lo then raise Domain else fn v => lo <= v andalso v <= hi
end
