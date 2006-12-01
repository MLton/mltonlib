(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure With :> WITH = struct
   open With

   infix >>= >>&

   val return = Fn.pass
   fun (wA >>= a2wB) f = wA (fn a => a2wB a f)

   fun alloc g a f = f (g a)
   fun free ef x f = (f x handle e => (ef x ; raise e)) before ef x

   fun (wA >>& wB) f = wA (fn a => wB (fn b => f (Product.& (a, b))))
   fun around new del = alloc new () >>= free del
   fun entry ef = alloc ef ()
   fun exit ef = free ef ()
   local
      fun `f x () = f x
   in
      fun calling {entry, exit} v = around (`entry v) (`exit v)
      fun passing ef {entry, exit} = around (`ef entry) (`ef exit)
   end
end
