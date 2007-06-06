(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Lifting :> LIFTING = struct
   (* <-- SML/NJ workaround *)
   open Fn
   infixr 4 />
   (* SML/NJ workaround --> *)

   datatype ('t, 'u) t = IN of {get : 'u -> 't, update : 't UnOp.t -> 'u UnOp.t}
   fun out (IN t) = t

   val id = IN {get = id, update = id}

   fun get lifting = op o /> #get (out (lifting ()))
   fun update lifting = #update (out (lifting ()))

   val F = IN {get = Pair.fst, update = Pair.mapFst}
   val S = IN {get = Pair.snd, update = Pair.mapSnd}

   fun (IN {get = gF, update = uF}) ^ (IN {get = gS, update = uS}) =
       IN {get = gS o gF, update = uF o uS}
end
