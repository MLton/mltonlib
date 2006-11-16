(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor to make common sequence (array or vector) extensions.
 *)
functor MkSeqCommonExt (type 'a t
                        val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
                        val fromList : 'a list -> 'a t
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
