(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UnivRef :> UNIV = struct
   exception Univ

   datatype t =
      IN of {clear : Unit.t Effect.t,
             store : Unit.t Effect.t}

   fun mk deref = let
      val r = ref NONE
   in
      (fn a =>
          IN {clear = fn () => r := NONE,
              store = fn () => r := SOME a},
       fn IN {clear, store} =>
          deref ((store () ; !r) before clear ()))
   end

   structure Iso = struct
      type 'a t = ('a, t) Iso.t
      fun new () = mk (fn SOME ? => ? | NONE => raise Univ)
   end

   structure Emb = struct
      type 'a t = ('a, t) Emb.t
      fun new () = mk Fn.id
   end
end
