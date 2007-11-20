(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Sub (include ANY type x) : sig
   include ANY
   val its : (x -> 'r) -> 'a t -> 'r
end = struct
   type 'a t = ('a, x) Product.t t
   fun partOf d = mapSub (Product.mapFst ignore) d
   fun its f d = f (Product.snd (getSub d))
   val getSub = fn d => Product.fst (getSub d)
   val mapSub = fn f => mapSub (Product.mapFst f)
end
