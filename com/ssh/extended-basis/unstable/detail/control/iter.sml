(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Iter :> ITER = struct
   infix 1 <|> until when by
   infix 0 >>= &

   datatype product = datatype Product.product

   type 'a t = ('a, Unit.t) CPS.t

   structure Monad =
      MkMonadP (type 'a monad = 'a t
                open CPS
                val zero = ignore
                fun a <|> b = b o Effect.obs a)
   open Monad

   fun unfold g s f =
       case g s of NONE => () | SOME (x, s) => (f x : Unit.t ; unfold g s f)

   exception S
   fun (m until p) f = m (fn x => if p x then raise S else f x) handle S => ()

   fun index m f = (fn i => m (fn a => f (a & !i before i := !i+1))) (ref 0)

   fun iterate f = unfold (fn x => SOME (x, f x))

   fun m when p = m >>= (fn x => if p x then return x else zero)
   fun m by f = map f m

   fun subscript b = if b then () else raise Subscript

   val up = iterate (fn x => x+1)
   fun upToBy l u d =
       (subscript (l <= u andalso 0 < d)
      ; unfold (fn l => if l<u then SOME (l, l+d) else NONE) l)
   fun upTo l u = upToBy l u 1

   val down = unfold (fn x => SOME (x-1, x-1))
   fun downToBy u l d =
       (subscript (l <= u andalso 0 < d)
      ; unfold (fn u => if l<u then SOME (u-d, u-d) else NONE) u)
   fun downTo u l = downToBy u l 1

   fun inList s = unfold List.getItem s

   fun inArraySlice s = unfold BasisArraySlice.getItem s
   fun inVectorSlice s = unfold BasisVectorSlice.getItem s

   fun inArray s = Fn.flip Array.app s
   fun inVector s = Fn.flip Vector.app s

   val inCharArraySlice = unfold BasisCharArraySlice.getItem
   val inCharVectorSlice = unfold BasisCharVectorSlice.getItem
   val inSubstring = inCharVectorSlice
   val inWord8ArraySlice = unfold BasisWord8ArraySlice.getItem
   val inWord8VectorSlice = unfold BasisWord8VectorSlice.getItem

   val inCharArray = Fn.flip CharArray.app
   val inCharVector = Fn.flip CharVector.app
   val inString = inCharVector
   val inWord8Array = Fn.flip Word8Array.app
   val inWord8Vector = Fn.flip Word8Vector.app

   val for = Fn.id
   fun fold f s m = (fn s => (m (fn x => s := f (x, !s)) : Unit.t ; !s)) (ref s)
   fun reduce zero plus one = fold plus zero o map one
   fun find p m = let
      exception S of 'a
   in
      NONE before m (fn x => if p x then raise S x else ()) handle S x => SOME x
   end
   fun collect m = rev (fold op :: [] m)
end