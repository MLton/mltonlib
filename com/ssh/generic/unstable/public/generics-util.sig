(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Utilities for defining generic functions.
 *)
signature GENERICS_UTIL = sig
   val failExn : Exn.t -> 'a
   val failExnSq : Exn.t Sq.t -> 'a
end
