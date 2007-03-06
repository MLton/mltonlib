(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UnivRef :> UNIV = struct
   open Univ

   datatype t =
      IN of {clear : unit -> unit,
             store : unit -> unit}

   local
      fun mk deref = let
         val r = ref NONE
      in
         (fn a =>
             IN {clear = fn () => r := NONE,
                 store = fn () => r := SOME a},
          fn IN {clear, store} =>
             deref ((store () ; !r) before clear ()))
      end
   in
      fun newIso () = mk (fn SOME ? => ? | NONE => raise Univ)
      fun newEmb () = mk (fn ? => ?)
   end
end

structure Univ :> UNIV = UnivRef
