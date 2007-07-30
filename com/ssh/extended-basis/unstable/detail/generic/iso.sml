(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Iso :> ISO = struct
   open Iso

   infix <-->

   val id = (Fn.id, Fn.id)

   val to = Pair.fst
   val from = Pair.snd
   val swap = Pair.swap

   fun (a2b, b2a) <--> (c2a, a2c) = (a2b o c2a, a2c o b2a)

   fun map (l, r) iso = r <--> iso <--> l
end
