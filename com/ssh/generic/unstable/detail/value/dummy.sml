(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDummy (Arg : OPEN_GENERIC) : DUMMY_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  0 &
   (* SML/NJ workaround --> *)

   structure Dummy =
      LayerGenericRep (structure Outer = Arg.Rep
                       structure Closed = MkClosedGenericRep (Thunk))

   open Dummy.This

   exception Dummy of Exn.t

   fun dummy a = getT a () handle e => raise Dummy e
   fun withDummy v = mapT (const (fn () => valOf v))

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Dummy and Rep = Dummy.Closed

      fun iso b (_, b2a) = b2a o b
      val isoProduct = iso
      val isoSum     = iso

      val op *`  = Product.thunk
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun a +` b = fn () => INL (a ()) handle _ => INR (b ())
      val unit = fn () => ()
      fun C0 _ = unit
      fun C1 _ = id
      val data = id

      val Y = Tie.function

      fun op --> _ = fn () => failing "Dummy.-->"

      val exn = fn () => Empty
      fun regExn _ _ = ()

      fun array  _ = Array.empty
      fun vector _ = Vector.empty
      fun list   _ = fn () => []

      fun refc a = ref o a

      val largeInt  = fn () => 0   : LargeInt.t
      val largeReal = fn () => 0.0 : LargeReal.t
      val largeWord = fn () => 0w0 : LargeWord.t

      val bool   = fn () => false
      val char   = fn () => #"\000"
      val int    = fn () => 0
      val real   = fn () => 0.0
      val string = fn () => ""
      val word   = fn () => 0w0

      val word8  = fn () => 0w0 : Word8.t
      val word32 = fn () => 0w0 : Word32.t
      val word64 = fn () => 0w0 : Word64.t)

   open Layered
end
