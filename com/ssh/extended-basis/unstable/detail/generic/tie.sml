(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Tie :> TIE = struct
   type 'a dom = Unit.t
   type 'a cod = 'a * 'a UnOp.t
   type 'a t = 'a dom -> 'a cod
   fun fix a f = let val (a, ta) = a () in ta (f a) end
   val pure = Fn.id
   fun iso tb iso = Pair.map (Iso.from iso, Fn.map iso) o tb
   fun op *` ab = Pair.map (Product.&, Product.map) o
                  Pair.swizzle o Pair.map ab o Sq.mk
   (* The rest are not primitive operations. *)
   fun tuple2 ab = iso (op *` ab) Product.isoTuple2
   fun tier th = pure ((fn (a, ua) => (a, Fn.const a o ua)) o th)
   fun option ? = pure (Fn.const (NONE, Fn.id)) ?
   fun fromRef rf x = !rf x
   fun function ? =
       tier (fn () => Pair.map (fromRef, Fn.curry op :=)
                               (Sq.mk (ref (Basic.raising Fix.Fix)))) ?
end
