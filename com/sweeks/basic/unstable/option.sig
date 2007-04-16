(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature OPTION = sig
   
   datatype 'a t =
      None
    | Some of 'a
   (**
    * The standard option type (with correct capitalization).
    *)

   val for: 'a t * ('a -> Unit.t) -> Unit.t
   val isNone: 'a t -> Bool.t
   (**
    * isNone None = true
    *)
   val isSome: 'a t -> Bool.t
   (**
    * isSome (Some x) = true
    *)
   val map: 'a t * ('a -> 'b) -> 'b t
   (**
    * map (None, f) = None
    * map (Some x, f) = Some (f x)
    *)
   val toSeq: 'a t -> 'a Seq.t
   (**
    * toSeq None returns an empty sequence.
    * toSeq (Some x) returns the sequence [x].
    *)
   val valOf: 'a t -> 'a
   (**
    * valOf None = raise Option
    * valOf (Some x) = x
    *)

end
