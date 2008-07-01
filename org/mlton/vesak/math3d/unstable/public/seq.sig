(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SEQ_CORE = sig
   type 'a t
   val t : 'a Generic.Rep.t -> 'a t Generic.Rep.t
   val findSome : ('a -> 'b Option.t) -> 'a t -> 'b Option.t
   val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val map : ('a -> 'b) -> 'a t -> 'b t
   val selector : ('a t -> 'a) t
end

signature SEQ = sig
   include SEQ_CORE
   val all : 'a UnPr.t -> 'a t UnPr.t
   val app : 'a Effect.t -> 'a t Effect.t
   val dup : 'a -> 'a t
   val new : 'a Thunk.t -> 'a t
   val exists : 'a UnPr.t -> 'a t UnPr.t
   val find : 'a UnPr.t -> 'a t -> 'a Option.t
   val for : 'a t -> 'a Effect.t Effect.t
   val sumWith : 'a BinOp.t -> 'a t -> 'a
   val toList : 'a t -> 'a List.t
   val zip : 'a t * 'b t -> ('a * 'b) t
   val zipWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   type 'a r
   val sub : ('a r t -> 'a r) -> 'a t -> 'a
   val update : ('a r t -> 'a r) -> 'a t * 'a -> 'a t
end
