(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   val assoc : (String.t * Int.t) List.t Ref.t = ref []
in
   fun bind (k, v) = assoc := (k, v) :: List.filter (notEq k o #1) (!assoc)
   fun find k = Option.map #2 (List.find (eq k o #1) (!assoc))
   fun bindings () = !assoc
end

val () = let
   open Server
   val procMap = ProcMap.new ()
   fun add ? = ProcMap.add procMap ?
in
   add (Pair.t (String.t, Int.t), Unit.t, "bind") bind
 ; add (String.t, Option.t Int.t, "find") find
 ; add (Unit.t, List.t (Pair.t (String.t, Int.t)), "bindings") bindings
 ; TCP.start procMap let
      open TCP.Opts
   in
      default
       & maxAccepts := SOME 1
   end
 ; run ()
end
