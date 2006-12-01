(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Exn : EXN = struct
   open Exn Ext.Exn
   val name = General.exnName
   val message = General.exnMessage
   fun apply f x = Sum.INR (f x) handle e => Sum.INL e
   fun eval th = apply th ()
   fun throw e = raise e
   fun try (th, fv, fe) = Sum.sum (fe, fv) (eval th)
   fun finally (th, ef) = try (th, Effect.past ef, throw o Effect.past ef)
end
