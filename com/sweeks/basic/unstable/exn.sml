(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Exn: EXN = struct

   type t = Exn.t

   datatype 'a z = Ok of 'a | Raise of t

   val run: (Unit.t -> 'a) -> 'a z =
      fn t => Ok (t ()) handle e => Raise e

   val eval: 'a z -> 'a =
      fn z =>
      case z of
         Ok x => x
       | Raise e => raise e

   val try: (Unit.t -> 'a) * ('a -> 'b) * (t -> 'b) -> 'b =
      fn (t, k, h) =>
      case run t of
         Ok x => k x
       | Raise e => h e

   fun finally (t, cleanup: Unit.t -> Unit.t) = let
      val z = run t
      val () = cleanup ()
   in
      eval z
   end

end

local
   open Exn
in
   val finally = finally
end
