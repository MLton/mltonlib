(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Lazy :> LAZY = struct
   datatype 'a status = LAZY of 'a t Thunk.t
                      | EAGER of (Exn.t, 'a) Sum.t
   withtype 'a t = 'a status ref ref
   fun lazy th = ref (ref (LAZY th))
   fun eager x = ref (ref (EAGER (Sum.INR x)))
   fun delay th = lazy (ref o ref o EAGER o (fn () => Exn.eval th))
   fun force promise =
       case !(!promise) of
          EAGER x => Exn.reflect x
        | LAZY th =>
          Exn.try (th,
                   fn promise' =>
                      case !(!promise) of
                         LAZY _ => (!promise := !(!promise')
                                  ; promise := !promise'
                                  ; force promise)
                       | EAGER x => Exn.reflect x,
                   fn e =>
                      (!promise := EAGER (Sum.INL e) (* XXX *)
                     ; raise e))

   fun toThunk promise = fn () => force promise
   fun memo th = toThunk (delay th)
   fun tie s k =
       case !(!s) of
          EAGER _ => raise Fix.Fix
        | LAZY _ => s := !k
   fun Y ? =
       Tie.tier (fn () => Pair.map (Fn.id, tie)
                                   (Sq.mk (lazy (Basic.raising Fix.Fix)))) ?
end
