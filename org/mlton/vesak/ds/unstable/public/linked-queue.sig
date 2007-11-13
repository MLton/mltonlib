(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature LINKED_QUEUE = sig
   include QUEUE
   val filter : 'a UnPr.t -> 'a t Effect.t
   val filterOut : 'a UnPr.t -> 'a t Effect.t
end
