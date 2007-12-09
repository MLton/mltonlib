(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure GenericsUtil :> GENERICS_UTIL = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   val ` = Exn.name
   fun failExn e = fails ["unregistered exn ", `e]
   fun failExnSq (l, r) = fails ["unregistered exns ", `l, " and ", `r]
end
