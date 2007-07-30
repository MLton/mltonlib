(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnivExn :> UNIV = struct
   exception Univ

   type t = Exn.t

   structure Iso = struct
      type 'a t = ('a, t) Iso.t
      fun new () = let
         exception U of 'a
      in
         (U, fn U ? => ? | _ => raise Univ)
      end
   end

   structure Emb = struct
      type 'a t = ('a, t) Emb.t
      fun new () = let
         exception U of 'a
      in
         (U, fn U ? => SOME ? | _ => NONE)
      end
   end
end
