(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   val assoc : (String.t * Int.t) List.t Ref.t = ref []
in
   fun bind (k, v) = assoc := (k, v) :: List.filter (notEq k o #1) (!assoc)
   fun bindings () = !assoc
   fun find k = Option.map #2 (List.find (eq k o #1) (!assoc))
end

val () = let
   open Server
   val procMap = ProcMap.new ()
   fun ` f s = ProcMap.add procMap s (verbose "server: " s f)
in
   mkLib {bind = `bind, bindings = `bindings, find = `find} >| ignore
 ; TCP.start procMap let
      open TCP.Opts
   in
      default
       & numAccepts := SOME 1
   end
 ; run ()
end
