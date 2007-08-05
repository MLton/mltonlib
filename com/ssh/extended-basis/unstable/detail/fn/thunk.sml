(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Thunk :> THUNK = struct
   open Thunk
   val mk = Fn.const
   fun map a2b a = a2b o a
   val isoValue = (fn th => th (), mk)
   fun iso (a2b, b2a) = (map a2b, map b2a)
end
