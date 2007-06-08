(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Dummy :> DUMMY_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open Basic Fn Product Sum
   infix  7 *`
   infix  6 +`
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure Lifted = LiftGeneric
     (structure Index = struct
         type 'a t = 'a Option.t
         type 'a s = 'a t
         type ('a, 'k) p = 'a t
      end

      fun iso b = flip Option.map b o Iso.from

      fun a *` b = case a & b of
                      SOME a & SOME b => SOME (a & b)
                    | _ => NONE

      fun a +` b = case a of
                      SOME a => SOME (INL a)
                    | NONE => Option.map INR b

      fun Y ? = Tie.pure (const (NONE, id)) ?

      local
         val e = Fail "Dummy.-->"
      in
         fun _ --> _ = SOME (raising e)
      end

      val exn = SOME Empty
      fun regExn _ _ = ()

      fun array _ = SOME (Array.tabulate (0, undefined))
      fun refc ? = Option.map ref ?

      fun vector _ = SOME (Vector.tabulate (0, undefined))

      val largeInt  : LargeInt.t  Index.t = SOME 0
      val largeReal : LargeReal.t Index.t = SOME 0.0
      val largeWord : LargeWord.t Index.t = SOME 0w0

      fun list _ = SOME []

      val bool   = SOME false
      val char   = SOME #"\000"
      val int    = SOME 0
      val real   = SOME 0.0
      val string = SOME ""
      val unit   = SOME ()
      val word   = SOME 0w0

      val word8  : Word8.t  Index.t = SOME 0w0
   (* val word16 : Word16.t Index.t = SOME 0w0 (* Word16 not provided by SML/NJ *) *)
      val word32 : Word32.t Index.t = SOME 0w0
      val word64 : Word64.t Index.t = SOME 0w0

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

   open Lifted

   structure Dummy = Index
   exception Dummy

   fun dummy (vo, _) =
       case vo of
          SOME v => v
        | NONE   => raise Dummy

   fun noDummy (_, x) = (NONE, x)
end

functor WithDummy (Outer : EXT_GENERIC) : DUMMY_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Outer and Inner = Dummy)
   open Dummy Joined
   structure Dummy = Index
   val dummy = fn ? => dummy (Outer.Index.getT ?)
   val noDummy = fn ? => Outer.Index.mapT noDummy ?
end
