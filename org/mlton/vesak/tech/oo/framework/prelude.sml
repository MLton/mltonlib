(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* NOTE: All of these are already provided by the Extended Basis library. *)

fun id x = x

structure Int = struct
   open Int
   type t = int
end

structure Unit = struct
   type t = unit
end

structure Sq = struct
   type 'a t = 'a * 'a
end

structure Effect = struct
   type 'a t = 'a -> Unit.t
end

structure Thunk = struct
   type 'a t = Unit.t -> 'a
end

infix &

structure Product = struct
   datatype ('a, 'b) product = & of 'a * 'b
   type ('a, 'b) t = ('a, 'b) product
   fun fst (a & _) = a
   fun snd (_ & b) = b
   fun mapFst f (a & b) = f a & b
end

datatype product = datatype Product.product
