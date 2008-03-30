(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for programming in continuation passing -style monadically.
 *)
signature CPS = sig
   type ('a, 'c) t = ('a -> 'c) -> 'c
   (** Type of CPS functions. *)

   val return : 'a -> ('a, 'c) t
   (** Return passes value to continuation: {return x = fn k => k x}. *)

   val >>= : ('a, 'c) t * ('a -> ('b, 'c) t) -> ('b, 'c) t
   (** Bind chains CPS blocks: {aM >>= a2bM = fn k => aM (fn a => a2bM a k)}. *)

   type ('a, 'c) cont
   (** Type of CPS -continuations. *)

   val callcc : (('a, 'c) cont, ('a, 'c) t) t
   (** Captures the current continuation and calls the function with it. *)

   val throw : ('a, 'c) cont -> 'a -> ('b, 'c) t
   (** Invokes a captured continuation with the given value. *)
end
