(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Effect :> EFFECT = struct
   open Effect
   val ignore = ignore
   val nop = ignore
   fun obs ef x = (ef x : Unit.t ; x)
   fun past ef x = (ef () : Unit.t ; x)
   fun tabulate n ef = ignore (Basic.repeat (fn i => (ef i : Unit.t ; i+1)) n 0)
   fun map b2a a = a o b2a
   fun iso (a2b, b2a) = (map b2a, map a2b)
end
