(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Contract :> CONTRACT = struct
   type 'a t = 'a UnOp.t
   exception Contract
   exception Caller of Exn.t and Callee of Exn.t
   val assert = Fn.id
   val any = Fn.id
   fun none _ = raise Contract
   val ef = Effect.obs
   fun pr pr x = if pr x then x else raise Contract
   fun op --> (d, c) f x =
       Exn.try (fn () => d x,
                fn x => c x (f x)
                   handle Caller e => raise Callee e
                        | Callee e => raise Caller e
                        | e        => raise Callee e,
                fn e as Caller _ => raise e
                 | e as Callee _ => raise e
                 | e             => raise Caller e)
   fun op andAlso (a, b) = b o a
end
