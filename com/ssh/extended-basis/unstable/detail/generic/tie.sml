(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Tie :> TIE = struct
   open Product
   infix &
   type 'a etaexp_dom = Unit.t
   type 'a etaexp_cod = ('a * 'a UnOp.t) Thunk.t
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod
   type 'a t = 'a etaexp
   fun fix aT f = let val (a, ta) = aT () () in ta (f a) end
   val pure = Thunk.mk
   fun iso bT (iso as (_, b2a)) () () = let
      val (b, fB) = bT () ()
   in
      (b2a b, Fn.map iso fB)
   end
   fun product (aT, a2bT) () () = let
      val (a, fA) = aT () ()
      val (b, fB) = a2bT a () ()
   in
      (a & b, Product.map (fA, fB))
   end
   (* The rest are not primitive operations. *)
   fun op *` (aT, bT) = product (aT, Fn.const bT)
   fun tuple2 ab = iso (op *` ab) Product.isoTuple2
   fun tier th = pure ((fn (a, ua) => (a, Fn.const a o ua)) o th)
   fun id x = pure (Fn.const (x, Fn.id))
   fun function ? =
       pure (fn () => let
                   val r = ref (Basic.raising Fix.Fix)
                in
                   (fn x => !r x, fn f => (r := f ; f))
                end) ?
end
