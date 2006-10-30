(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
   
signature STREAM =
   sig
      type 'a t

      val append: 'a t * 'a t -> 'a t
      val appends: 'a t t -> 'a t
      val delay: (unit -> ('a t * 'a) option) -> 'a t
      val delay': (unit -> 'a t) -> 'a t
      val empty: 'a t
      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val exists: 'a t * ('a -> bool) -> bool
      val firstN: 'a t * int -> 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foreach: 'a t * ('a -> unit) -> unit
      val get: 'a t -> ('a t * 'a) option
      val isEmpty: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val length: 'a t -> int
      val map: 'a t * ('a -> 'b) -> 'b t
      val memo: (unit -> ('a t * 'a) option) -> 'a t
      val peek2: 'a t * 'b t * ('a * 'b -> bool) -> ('a * 'b) option
      val prefix: 'a t * 'a -> 'a t
      val singleton: 'a -> 'a t
   end
