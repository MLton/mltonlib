(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure With :> WITH = struct
   type 'a t = 'a Effect.t Effect.t

   infix >>=

   structure Monad =
      MkMonad (type 'a monad = 'a t
               val return = Fn.pass
               fun (aM >>= a2bM) f = aM (fn a => a2bM a f))

   open Monad

   val lift = Fn.id
   val for = Fn.id
   fun one aM f = let
      val result = ref NONE
   in
      aM (fn a => result := SOME (f a)) : Unit.t
    ; valOf (!result)
   end

   fun alloc g a f = f (g a)
   fun free ef x f = (f x handle e => (ef x ; raise e)) before ef x

   fun around new del = alloc new () >>= free del
   fun entry ef = alloc ef ()
   fun exit ef = free ef ()
   local
      fun `f x () = f x
   in
      fun calling {entry, exit} v = around (`entry v) (`exit v)
      fun passing ef {entry, exit} = around (`ef entry) (`ef exit)
   end
end
