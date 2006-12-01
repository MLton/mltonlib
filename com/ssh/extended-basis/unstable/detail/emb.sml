(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Emb :> EMB = struct
   open Emb

   infix <-->

   val id = (Fn.id, SOME)

   val to = Pair.fst
   val from = Pair.snd

   fun (a2b, b2aOpt) <--> (c2a, a2cOpt) =
       (a2b o c2a, Option.composePartial (a2cOpt, b2aOpt))
end
