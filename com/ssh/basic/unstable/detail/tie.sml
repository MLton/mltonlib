(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Tie :> TIE = struct
   type 'a t_domain = unit
   type 'a t_range = 'a * 'a UnOp.t
   type 'a t = 'a t_domain -> 'a t_range
   fun fix a f = let val (a, ta) = a () in ta (f a) end
   val pure = Fn.id
   fun tier th = (fn (a, ta) => (a, Fn.const a o ta)) o th
   fun iso tb iso = Pair.map (Iso.from iso, Fn.map iso) o tb
   fun op *` (a, b) = Pair.map (Product.&, Product.map) o
                      Pair.swizzle o Pair.map (a, b) o Sq.mk
   fun tuple2 (a, b) = iso (op *` (a, b)) Product.isoTuple2
   fun option () = (NONE, Fn.id)
   fun fromRef rf x = !rf x
   fun function ? =
       tier (fn () => Pair.map (fromRef, Fn.curry op :=)
                               (Sq.mk (ref (Fn.failing Fix.Fix)))) ?
end
