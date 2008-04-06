(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Iter :> ITER = struct
   open Exn Option Product UnPr Effect Fn

   infix 1 <|>
   infix 0 >>= &

   type 'a t = 'a Effect.t Effect.t

   structure Monad =
      MkMonadP (type 'a monad = 'a t
                open CPS
                val zero = ignore
                fun a <|> b = b o obs a)
   open Monad

   fun intersperse x aM e =
       case ref true
        of isFirst =>
           aM (fn a => (if !isFirst then isFirst := false else e x ; e a))

   fun on i e = map (obs e) i

   fun unfold g s f =
       case g s of NONE => () | SOME (x, s) => (f x : Unit.t ; unfold g s f)

   fun until p m f = let
      exception S
   in
      m (fn x => if p x then raise S else f x) handle S => ()
   end

   fun until' p m f = let
      exception S
   in
      m (fn x => (f x : Unit.t ; if p x then raise S else ())) handle S => ()
   end

   fun whilst p = until (neg p)
   fun whilst' p = until' (neg p)

   fun subscript b = if b then () else raise Subscript

   fun take n =
       (subscript (0 <= n)
      ; fn m => fn f => case ref n of n =>
           if !n <= 0 then () else until' (fn _ => (n := !n-1 ; !n <= 0)) m f)

   fun iterate f = unfold (fn x => SOME (x, f x))

   fun repeat x = iterate id x
   fun replicate n =
       (subscript (0 <= n)
      ; fn x => unfold (fn 0 => NONE | n => SOME (x, n-1)) n)
   fun cycle m f = (m f : Unit.t ; cycle m f)

   type ('f, 't, 'b) mod = 'f * 't * 'b

   fun From ? = Fold.mapSt1 (fn f => fn (_, t, b) => (f, t, b)) ?
   fun To   ? = Fold.mapSt1 (fn t => fn (f, _, b) => (f, t, b)) ?
   fun By   ? = Fold.mapSt1 (fn b => fn (f, t, _) => (f, t, b)) ?

   fun up ? = Fold.wrap ((0, (), 1), fn (l, (), s) =>
       (subscript (0 < s) ; iterate (fn l => l+s) l)) ?

   fun down ? = Fold.wrap ((0, (), 1), fn (u, (), s) =>
       (subscript (0 < s) ; iterate (fn u => u-s) (u-s))) ?

   fun upTo u = Fold.wrap ((0, u, 1), fn (l, u, s) =>
       (subscript (l = u orelse 0 < s)
      ; unfold (fn l => if l < u then SOME (l, l+s) else NONE) l))

   fun downFrom u = Fold.wrap ((u, 0, 1), fn (u, l, s) =>
       (subscript (l = u orelse 0 < s)
      ; unfold (fn u => if l < u then SOME (u-s, u-s) else NONE) u))

   val integers = up Fold.$

   fun index ? = Fold.wrap ((0, (), 1), fn (i, (), d) =>
    fn m => fn f => (fn i => m (fn a => f (a & !i) before i := !i+d)) (ref i)) ?

   val maxRealInt = Real.Math.pow (2.0, Real.fromInt Real.precision)

   fun realsTo e = Fold.wrap ((0.0, (), 1.0), fn (b, (), s) => let
      val n = (e-b)/s
      val n = if 0.0 <= n andalso n <= maxRealInt then n else
              if n < 0.0 then 0.0
              else raise Domain
   in
      unfold (fn i => if i < n then SOME (i*s + b, i+1.0) else NONE) 0.0
   end)

   fun inList s = unfold List.getItem s
   fun onList s = unfold (fn [] => NONE | l as _::t => SOME (l, t)) s

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

   fun inImperativeStream openS closeS readS a e = let
      val s = openS a
      fun lp () = case readS s of NONE => () | SOME x => (e x : Unit.t ; lp ())
   in
      after (lp, fn () => closeS s)
   end

   local
      open BasisTextIO
   in
      val lines = inputLine
      val chars = input1
      fun inTextFile f = Fold.wrap (((), (), chars), fn ((), (), input) =>
          inImperativeStream openIn closeIn input f)
   end
   val inDir = let
      open BasisOS.FileSys
   in
      inImperativeStream openDir closeDir readDir
   end

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
   fun all p = isNone o find (neg p)
   fun exists p = isSome o find p
end
