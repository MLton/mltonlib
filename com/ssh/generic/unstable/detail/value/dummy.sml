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

      fun a *` b = fn () => a () & b ()
      fun a +` b = fn () => INL (a ()) handle _ => INR (b ())

      val Y = Tie.function

      fun op --> _ = fn () => failing "Dummy.-->"

      val exn = fn () => Empty
      fun regExn _ _ = ()

      fun array _ = fn () => Array.tabulate (0, undefined)
      fun refc a = ref o a

      fun vector _ = fn () => Vector.tabulate (0, undefined)

      val largeInt  = fn () => 0   : LargeInt.t
      val largeReal = fn () => 0.0 : LargeReal.t
      val largeWord = fn () => 0w0 : LargeWord.t

      fun list _ = fn () => []

      val bool   = fn () => false
      val char   = fn () => #"\000"
      val int    = fn () => 0
      val real   = fn () => 0.0
      val string = fn () => ""
      val unit   = fn () => ()
      val word   = fn () => 0w0

      val word8  = fn () => 0w0 : Word8.t
   (* val word16 = fn () => 0w0 : Word16.t (* Word16 not provided by SML/NJ *) *)
      val word32 = fn () => 0w0 : Word32.t
      val word64 = fn () => 0w0 : Word64.t

      (* Trivialities *)

      val isoProduct = iso
      val isoSum = iso

      val T = id
      fun R _ = id
      val tuple = id
      val record = id

      fun C0 _ = unit
      fun C1 _ = id
      val data = id)

   open Layered
end
