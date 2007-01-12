(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Utility module for lifting type-indexed values.
 *)

structure Lift :> sig
   type ('t, 'u) t

   val id : ('a, 'a) t

   val get : ('a, 'b) t Thunk.t -> ('a -> 'c) -> 'b -> 'c

   val update : ('a, 'b) t Thunk.t -> 'a UnOp.t -> 'b UnOp.t

   val A : ('a, 'a * 'b) t
   val B : ('b, 'a * 'b) t

   val ^ : ('m, 'u) t * ('t, 'm) t -> ('t, 'u) t
end = struct
   datatype ('t, 'u) t = IN of {get : 'u -> 't, update : 't UnOp.t -> 'u UnOp.t}
   fun out (IN t) = t

   val id = IN {get = id, update = id}

   fun get lift = op o /> #get (out (lift ()))
   fun update lift = #update (out (lift ()))

   val A = IN {get = Pair.fst, update = Pair.mapFst}
   val B = IN {get = Pair.snd, update = Pair.mapSnd}

   fun (IN {get = gL, update = uL}) ^ (IN {get = gR, update = uR}) =
       IN {get = gR o gL, update = uL o uR}
end
