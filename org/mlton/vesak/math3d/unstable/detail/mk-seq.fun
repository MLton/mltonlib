(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkSeq (Arg : SEQ_CORE) : SEQ = struct
   open Arg
   fun find p = findSome (fn x => if p x then SOME x else NONE)
   fun all p = isNone o find (neg p)
   fun exists p = isSome o find p
   fun zipWith f (l, r) = let
      val l = map INL l
      val r = map INR r
   in
      map (fn s => f (Sum.outL (s l), Sum.outR (s r))) selector
   end
   fun zip ? = zipWith id ?
   fun app e = ignore o map e
   fun for s e = app e s
   fun dup v = map (const v) selector
   fun new th = map (th o ignore) selector
   fun toList v = foldr op :: [] v
   type 'a r = 'a ref
   fun sub f v = case map ref v of r => ! (f r)
   fun update f (v, s) = case map ref v of r => (f r := s ; map ! r)
   fun sumWith f =
       Sum.outR o foldr (fn (v, INL ()) => INR v
                          | (v, INR s) => INR (f (s, v))) (INL ())
end
