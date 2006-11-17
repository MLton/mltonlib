(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Effect :> EFFECT = struct
   type 'a t = 'a -> unit
   val ignore = ignore
   val nop = ignore
   fun obs ef x = (ef x : unit ; x)
   fun past ef x = (ef () : unit ; x)
end
