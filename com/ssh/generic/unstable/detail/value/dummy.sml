(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  0 &
   (* SML/NJ workaround --> *)

   structure Dummy : CLOSED_GENERIC = struct
      structure Rep = MkClosedGenericRep (Thunk)

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
      val data = id
   end

   structure Dummy : OPENED_GENERIC = OpenGeneric (Dummy)
in
   structure Dummy :> DUMMY_GENERIC = struct
      open Dummy
      structure Dummy = Rep
      exception Dummy of Exn.t
      val dummy : ('a, 'x) Dummy.t -> 'a =
          fn a => This.getT a () handle e => raise Dummy e
      fun withDummy v = This.mapT (const (fn () => valOf v))
   end
end

functor WithDummy (Arg : OPEN_GENERIC) : DUMMY_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Arg and Inner = Dummy)
   open Dummy Joined
   structure Dummy = Rep
   val dummy = fn ? => dummy (Arg.Rep.getT ?)
   val withDummy = fn v => fn ? => Arg.Rep.mapT (withDummy v) ?
end
