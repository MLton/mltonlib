(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Functor or Covariant Functor ==
 *
 * The concept of a functor comes from category theory.  Here a functor
 * consists of a type constructor {func} and a function {map}:
 *
 *> type 'a func
 *> val map : ('a -> 'b) -> 'a func -> 'b func
 *
 * Furthermore, the {map} function must obey two laws:
 *
 *> 1. map id == id
 *> 2. map (f o g) == map f o map g
 *)

signature FUNC = sig
   type 'a func
   val map : ('a -> 'b) -> 'a func -> 'b func
end

(** == Contravariant Functor == *)

signature CFUNC = sig
   type 'a func
   val map : ('b -> 'a) -> 'a func -> 'b func
end
