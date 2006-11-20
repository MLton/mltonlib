(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkMonoSeqCommonExt (type t
                            type elem
                            val foldr : (elem * 'a -> 'a) -> 'a -> t -> 'a
                            val fromList : elem list -> t
                            val maxLen : int) = struct
   fun unfoldi fis (n, s) = let
      fun lp (i, s, xs) =
          if i = n then (fromList (rev xs), s)
          else case fis (i, s) of (x, s) => lp (i+1, s, x::xs)
   in if n < 0 orelse maxLen < n then raise Size else lp (0, s, [])
   end
   fun toList t = foldr op :: [] t
   val isoList = (toList, fromList)
end
