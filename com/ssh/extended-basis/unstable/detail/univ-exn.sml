(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnivExn :> UNIV = struct
   open Univ

   type t = Exn.t

   fun newIso () = let
      exception U of 'a
   in
      (U, fn U ? => ? | _ => raise Univ)
   end

   fun newEmb () = let
      exception U of 'a
   in
      (U, fn U ? => SOME ? | _ => NONE)
   end
end

structure Univ :> UNIV = UnivExn
