(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkSeqCommonExt (type 'a t
                        val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
                        val fromList : 'a List.t -> 'a t
                        val maxLen : Int.t) = struct
   fun unfoldi fis (n, s) = let
      fun lp (i, s, xs) =
          if i = n then (fromList (rev xs), s)
          else case fis (i, s) of (x, s) => lp (i+1, s, x::xs)
   in if n < 0 orelse maxLen < n then raise Size else lp (0, s, [])
   end
   fun toList t = foldr op :: [] t
   val isoList = (toList, fromList)
end
