(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Fold :> FOLD = struct
   (* <-- SML/NJ workaround *)
   fun pass x k = k x
   fun id x = x
   structure Pair = struct
      fun map (f, g) (x, y) = (f x, g y)
      fun fst (x, _) = x
      fun snd (_, y) = y
   end
   (* SML/NJ workaround --> *)
   datatype ('a, 'b, 'c) t = T of 'a * ('b -> 'c)
   type ('s1, 's2, 'r) s = 's1 -> ('s2, 'r) CPS.t
   fun $ (T (t, f)) = f t
   fun wrap (t, f) = pass (T (t, f))
   fun unwrap f = f (fn T t => t)
   fun map g (T t) = pass (T (g t))
   (* The rest are not-primitive. *)
   type ('a, 's1, 's2, 'r) s1 = 's1 -> 'a -> ('s2, 'r) CPS.t
   fun post g = wrap o Pair.map (id, fn f => g o f) o unwrap
   fun unmap s t = wrap t s $
   fun map1 g ? x = map (g x) ?
   fun unmap1 s1 x t = wrap t s1 x $
   fun rewrap f = wrap (unwrap f)
   fun remap s = map (unmap s)
   fun remap1 s1 = map1 (unmap1 s1)
   fun mapFin g = map (Pair.map (id, g))
   fun mapSt g = map (Pair.map (g, id))
   fun mapSt1 g = map1 (fn x => Pair.map (g x, id))
   fun l f t = f o t
   fun r f t = t o f
   fun comFinL g = mapFin (l g)
   fun comFinR g = mapFin (r g)
   fun comStL g = mapSt (l g)
   fun comStR g = mapSt (r g)
   fun comStL1 g = mapSt1 (l o g)
   fun comStR1 g = mapSt1 (r o g)
   structure NSZ = struct
      datatype ('a, 'b, 'c, 'd, 'e, 'f, 'g) t' =
         T of 'a * (('b -> 'c) * ('d -> 'e) -> 'f -> 'g)
      val wrap = fn {zero, none, some} =>
          wrap (T (zero, Pair.fst), fn T (ac, get) => get (none, some) ac)
      val mapSt = fn {none, some} =>
          mapSt (fn T (ac, get) => T (get (none, some) ac, Pair.snd))
      val mapSt1 = fn {none, some} =>
          mapSt1 (fn x => fn T (ac, get) => T (get (none, some) x ac, Pair.snd))
   end
end
