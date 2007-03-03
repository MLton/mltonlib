(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of a type-indexed family of dummy values.  In Standard
 * ML, dummy values are needed for things such as computing fixpoints and
 * building cyclic values.
 *
 * This type-indexed function is unlikely to be directly useful in
 * application programs and is more likely to be used internally in the
 * implementation of some other type-indexed functions (e.g. pickling).
 *)

signature DUMMY = sig
   type 'a dummy_t

   exception Dummy
   (**
    * This is raised when trying to extract the dummy value in case of
    * unfounded recursion or an abstract type that has not been given a
    * dummy value.
    *)

   val dummy : 'a dummy_t -> 'a
   (** Extracts the dummy value or raises {Dummy}. *)

   val noDummy : 'a dummy_t UnOp.t
   (**
    * Removes the dummy value from the given type-index.  This can be used
    * for encoding abstract types that can not be given dummy values.
    *)
end

functor LiftDummy
           (include DUMMY
            type 'a t
            val lift : ('a dummy_t, 'a t) Lift.t Thunk.t) : DUMMY = struct
   type 'a dummy_t = 'a t
   exception Dummy = Dummy
   val dummy   = fn ? => Lift.get    lift dummy   ?
   val noDummy = fn ? => Lift.update lift noDummy ?
end

structure Dummy :> sig
   include STRUCTURAL_TYPE
   include DUMMY where type 'a dummy_t = 'a t
end = struct
   type 'a t = 'a option
   type 'a dummy_t = 'a t

   exception Dummy

   val dummy = fn SOME v => v
                | NONE => raise Dummy

   fun noDummy _ = NONE

   fun iso b = flip Option.map b o Iso.from

   fun a *` b = case a & b of
                   SOME a & SOME b => SOME (a & b)
                 | _ => NONE

   fun a +` b = case a of
                   SOME a => SOME (INL a)
                 | NONE => Option.map INR b

   val unit = SOME ()

   fun Y ? = Tie.pure (const (NONE, id)) ?

   local
      val e = Fail "Dummy.-->"
   in
      fun _ --> _ = SOME (raising e)
   end

   val exn = SOME Empty
   fun regExn _ _ = ()

   fun array _ = SOME (Array.tabulate (0, undefined))
   fun refc  ? = Option.map ref ?

   fun vector _ = SOME (Vector.tabulate (0, undefined))

   val largeInt  : LargeInt.int   t = SOME 0
   val largeReal : LargeReal.real t = SOME 0.0
   val largeWord : LargeWord.word t = SOME 0w0

   fun list _ = SOME []

   val bool   = SOME false
   val char   = SOME #"\000"
   val int    = SOME 0
   val real   = SOME 0.0
   val string = SOME ""
   val word   = SOME 0w0

   val word8  : Word8.word  t = SOME 0w0
   val word16 : Word16.word t = SOME 0w0
   val word32 : Word32.word t = SOME 0w0
   val word64 : Word64.word t = SOME 0w0
end
