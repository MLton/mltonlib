(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
functor Recur
   (S:
    sig
       type 'a t
       type 'a elem
       val recur: ('a t * 'b * ('b -> 'c) * ('a elem * 'b * ('b -> 'c) -> 'c)
                   -> 'c)
    end): ENUMERABLE = struct

   open S

   type 'a t0 = 'a t
      
   fun isEmpty s = recur (s, (), const true, const false)

   fun fold (s, b, f) = recur (s, b, id, fn (a, b, k) => k (f (a, b)))

   fun size s = fold (s, 0, fn (_, n) => n + 1)

   fun find (s, f) =
      recur (s, (), const None, fn (x, (), k) => if f x then Some x else k ())

   fun for (s, f) = fold (s, (), f o #1)

   fun all (s, f) = recur (s, (), const true, fn (x, (), k) => f x andalso k ())

   fun exists (s, f) = not (all (s, not o f))

   fun last s =
      recur (s, None,
             fn None => raise Empty | Some x => x,
             fn (x, _, k) => k (Some x))
          
   fun sub (s, n) =
      recur (s, n, fn _ => raise Subscript,
             fn (x, i, k) => if i = 0 then x else k (i - 1))

   fun toListR s = fold (s, [], op ::)

   fun toSeqR s = List.toSeq (toListR s)

   fun toSeq s = List.toSeq (List.reverse (toListR s))

end
