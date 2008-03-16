(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for utilities for programming in continuation passing -style.
 *)
signature CPS = sig
   type ('a, 'c) t = ('a -> 'c) -> 'c
   (** Type of CPS functions. *)

   val return : 'a -> ('a, 'c) t
   (** Pass to continuation: {return x f = f x}. *)

   val >>= : ('a, 'c) t * ('a -> ('b, 'c) t) -> ('b, 'c) t
   (** Bind. *)
end
