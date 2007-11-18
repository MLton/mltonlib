(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature ANY = sig
   type 'a t
   val part : 'a t -> Unit.t t
   val getSub : 'a t -> 'a
   val mapSub : ('a -> 'b) -> 'a t -> 'b t
end
