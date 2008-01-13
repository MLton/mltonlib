(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for programming in continuation passing -style. *)
signature CPS = sig
   type ('a, 'b) t = ('a -> 'b) -> 'b

   val pass : 'a -> ('a, 'b) t
   (** Pass to continuation ({pass x f = f x}). *)
end
