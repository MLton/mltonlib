(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Utilities for defining type-indexed functions.
 *)

structure TypeUtil :> sig
   val failExn : Exn.t -> 'a
   val failExnSq : Exn.t Sq.t -> 'a
end = struct
   val ` = Exn.name
   fun failCat ss = fail (concat ss)
   fun failExn e = failCat ["unregistered exn ", `e]
   fun failExnSq (l, r) = failCat ["unregistered exns ", `l, " and ", `r]
end
