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
      structure Rep = struct
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

      fun op --> _ = SOME (failing "Dummy.-->")

      val exn = SOME Empty
      fun regExn _ _ = ()

      fun array _ = SOME (Array.tabulate (0, undefined))
      fun refc ? = Option.map ref ?

      fun vector _ = SOME (Vector.tabulate (0, undefined))

      val largeInt  : LargeInt.t  Rep.t = SOME 0
      val largeReal : LargeReal.t Rep.t = SOME 0.0
      val largeWord : LargeWord.t Rep.t = SOME 0w0

      fun list _ = SOME []

      val bool   = SOME false
      val char   = SOME #"\000"
      val int    = SOME 0
      val real   = SOME 0.0
      val string = SOME ""
      val unit   = SOME ()
      val word   = SOME 0w0

      val word8  : Word8.t  Rep.t = SOME 0w0
   (* val word16 : Word16.t Rep.t = SOME 0w0 (* Word16 not provided by SML/NJ *) *)
      val word32 : Word32.t Rep.t = SOME 0w0
      val word64 : Word64.t Rep.t = SOME 0w0

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

   structure Dummy : OPEN_GENERIC = OpenGeneric (Dummy)
in
   structure Dummy :> DUMMY_GENERIC = struct
      open Dummy

      structure Dummy = Rep
      exception Dummy

      val dummy : ('a, 'x) Dummy.t -> 'a =
          fn (SOME v, _) => v
           | (NONE,   _) => raise Dummy

      fun noDummy (_, x) = (NONE, x)
   end
end

functor WithDummy (Arg : OPEN_GENERIC) : DUMMY_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Arg and Inner = Dummy)
   open Dummy Joined
   structure Dummy = Rep
   val dummy = fn ? => dummy (Arg.Rep.getT ?)
   val noDummy = fn ? => Arg.Rep.mapT noDummy ?
end
