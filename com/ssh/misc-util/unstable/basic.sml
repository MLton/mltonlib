(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Some basic combinators; the kind of combinators you would expect to see
 * in the language standard library or prelude.
 *)
structure Basic :> sig
   val repeat : 'a UnOp.t -> Int.t -> 'a UnOp.t
   (** {repeat f n x} repeats {f} {n}-times starting with {x}. *)

   val += : (Int.t Ref.t * Int.t) Effect.t
   (** {c += n} is equivalent to {c := !c + n}. *)

   val -= : (Int.t Ref.t * Int.t) Effect.t
   (** {c -= n} is equivalent to {c := !c - n}. *)
end = struct
   fun repeat f n x = if n = 0 then x else repeat f (n-1) (f x)

   fun c += n = c := !c + n
   fun c -= n = c := !c - n
end

(* Expose all of the basic combinators at the top-level for convenience. *)
open Basic
