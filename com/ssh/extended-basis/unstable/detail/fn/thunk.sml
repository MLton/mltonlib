(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Thunk :> THUNK = struct
   open Thunk
   val mk = Fn.const
   val iso = (mk, fn th => th ())
end
