(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Iter :> ITER = struct
   open Product UnPr Effect Fn

   infix 1 <|> whilst whilst' until until' when unless by
   infix 0 >>= &

   type 'a t = ('a, Unit.t) CPS.t

   structure Monad =
      MkMonadP (type 'a monad = 'a t
                open CPS
                val zero = ignore
                fun a <|> b = b o obs a)
   open Monad

   fun unfold g s f =
       case g s of NONE => () | SOME (x, s) => (f x : Unit.t ; unfold g s f)

   fun (m until p) f = let
      exception S
   in
      m (fn x => if p x then raise S else f x) handle S => ()
   end

   fun (m until' p) f = let
      exception S
   in
      m (fn x => (f x : Unit.t ; if p x then raise S else ())) handle S => ()
   end

   fun m whilst p = m until neg p
   fun m whilst' p = m until' neg p

   fun indexFromBy i d m f =
       (fn i => m (fn a => f (a & !i) before i := !i+d)) (ref i)
   fun indexFrom i = indexFromBy i 1
   fun index m = indexFrom 0 m

   fun iterate f = unfold (fn x => SOME (x, f x))

   fun m unless p = m >>= (fn x => if p x then zero else return x)
   fun m when p = m unless neg p

   fun m by f = map f m

   fun subscript b = if b then () else raise Subscript

   fun repeat x = iterate id x
   fun replicate n =
       (subscript (0 <= n)
      ; fn x => unfold (fn 0 => NONE | n => SOME (x, n-1)) n)
   fun cycle m f = (m f : Unit.t ; cycle m f)

   fun take n =
       (subscript (0 <= n)
      ; fn m => fn f => case ref n of n =>
           if !n <= 0 then () else (m until' (fn _ => (n := !n-1 ; !n <= 0))) f)

   val up = iterate (fn x => x+1)
   fun upToBy l u d =
       (subscript (l <= u andalso 0 < d)
      ; unfold (fn l => if l < u then SOME (l, l+d) else NONE) l)
   fun upTo l u = upToBy l u 1
   val down = unfold (fn x => SOME (x-1, x-1))
   fun downToBy u l d =
       (subscript (l <= u andalso 0 < d)
      ; unfold (fn u => if l < u then SOME (u-d, u-d) else NONE) u)
   fun downTo u l = downToBy u l 1
   val integers = up 0

   fun rangeBy f t d = let
      val op < = case Int.compare (f, t)
                  of LESS    => op <
                   | EQUAL   => op <>
                   | GREATER => op >
   in
      subscript (f = t orelse f < t andalso 0 < d)
    ; unfold (fn f => if f < t then SOME (f, f+d) else NONE) f
   end
   fun range f t = if f < t then rangeBy f t 1 else rangeBy f t ~1

   fun inList s = unfold List.getItem s

   fun inArraySlice s = unfold BasisArraySlice.getItem s
   fun inVectorSlice s = unfold BasisVectorSlice.getItem s

   fun inArray s = flip Array.app s
   fun inVector s = flip Vector.app s

   val inCharArraySlice = unfold BasisCharArraySlice.getItem
   val inCharVectorSlice = unfold BasisCharVectorSlice.getItem
   val inSubstring = inCharVectorSlice
   val inWord8ArraySlice = unfold BasisWord8ArraySlice.getItem
   val inWord8VectorSlice = unfold BasisWord8VectorSlice.getItem

   val inCharArray = flip CharArray.app
   val inCharVector = flip CharVector.app
   val inString = inCharVector
   val inWord8Array = flip Word8Array.app
   val inWord8Vector = flip Word8Vector.app

   val for = id
   fun fold f s m = (fn s => (m (fn x => s := f (x, !s)) : Unit.t ; !s)) (ref s)
   fun reduce zero plus one = fold plus zero o map one
   fun find p m = let
      exception S of 'a
   in
      NONE before m (fn x => if p x then raise S x else ()) handle S x => SOME x
   end
   fun collect m = rev (fold op :: [] m)
   fun first m = find (const true) m
   fun last m = fold (SOME o #1) NONE m
end
