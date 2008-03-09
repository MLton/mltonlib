(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithSome (Arg : WITH_SOME_DOM) : SOME_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 0 &
   (* SML/NJ workaround --> *)

   fun iso' b (_, b2a) = b2a o b

   structure SomeRep = LayerRep' (open Arg Thunk)

   open SomeRep.This

   exception Nothing of Exn.t

   fun some a = getT a () handle e => raise Nothing e
   fun withNone ? = mapT (const (raising Option)) ?
   fun withSome v = mapT (const (const v))

   structure Open = LayerDepCases
     (fun iso        ? = iso' (getT ?)
      fun isoProduct ? = iso' (getP ?)
      fun isoSum     ? = iso' (getS ?)

      fun op *` (a, b) = Product.thunk (getP a, getP b)
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val a = getS aS
         val b = getS bS
      in
         (* We are careful here to avoid diverging. *)
         case Arg.hasBaseCase aS & Arg.hasBaseCase bS
          of true  & false => INL o a
           | false & true  => INR o b
           | _             => fn () => INL (a ()) handle _ => INR (b ())
      end
      val unit = fn () => ()
      fun C0 _ = unit
      fun C1 _ = getT
      val data = getS

      fun Y ? = Tie.id (failing "degenerate type") ?

      fun op --> _ = fn () => failing "Some.-->"
      (* An alternative implementation would be
       *
       *> fun op --> (_, b) = fn () => getT b o ignore
       *
       * but it could mask defects where a dummy function is used by
       * mistake.
       *)

      val exn = fn () => Empty
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun array  _ = Array.empty
      fun vector _ = Vector.empty
      fun list   _ = fn () => []

      fun refc a = ref o getT a

      val fixedInt  = fn () => 0 : FixedInt.t
      val largeInt  = fn () => 0 : LargeInt.t

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
(*
      val word64 = fn () => 0w0 : Word64.t
*)

      fun hole () = undefined

      open Arg SomeRep)
end
