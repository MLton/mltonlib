(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Exn : EXN = struct
   open Exn Ext.Exn
   val name = BasisGeneral.exnName
   val message = BasisGeneral.exnMessage
   fun apply f x = Sum.INR (f x) handle e => Sum.INL e
   fun eval th = apply th ()
   fun throw e = raise e
   fun reflect s = Sum.sum (throw, Fn.id) s
   fun try (th, fv, fe) = Sum.sum (fe, fv) (eval th)
   fun after (th, ef) = try (th, Effect.past ef, throw o Effect.past ef)
   val finally = after
end
