(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Effect :> EFFECT = struct
   open Effect
   val ignore = ignore
   val nop = ignore
   fun obs ef x = (ef x : unit ; x)
   fun past ef x = (ef () : unit ; x)
end
