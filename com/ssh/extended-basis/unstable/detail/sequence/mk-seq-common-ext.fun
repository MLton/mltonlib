(* Copyright (C) 2008 Vesa Karvonen
 * Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkSeqCommonExt (type 'a t
                        val tabulate : Int.t * (Int.t -> 'a) -> 'a t
                        val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
                        val fromList : 'a List.t -> 'a t
                        val maxLen : Int.t
                        val app : 'a Effect.t -> 'a t Effect.t
                        val appi : (Int.t * 'a) Effect.t -> 'a t Effect.t) = struct
   fun empty () = tabulate (0, Basic.undefined)
   fun unfoldi fis (n, s) = let
      fun lp (i, s, xs) =
          if i = n then (fromList (rev xs), s)
          else case fis (i, s) of (x, s) => lp (i+1, s, x::xs)
   in if n < 0 orelse maxLen < n then raise Size else lp (0, s, [])
   end
   fun toList t = foldr op :: [] t
   val isoList = (toList, fromList)
   fun for xs ef = app ef xs
   fun fori xs ef = appi ef xs
end
