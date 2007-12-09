(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Basic utilities. *)
signature BASIC = sig
   val eq : ''a -> ''a UnPr.t
   (** Curried version of {=}. *)

   val notEq : ''a -> ''a UnPr.t
   (** Curried version of {<>}. *)

   val fail : String.t -> 'a
   (** {fail m} is equivalent to {raise Fail m}. *)

   val fails : String.t List.t -> 'a
   (** {fails ms} is equivalent to {fail (concat ms)}. *)

   val failing : String.t -> 'a -> 'b
   (** A failing function; {failing m} is equivalent to {raising (Fail m)}. *)

   val raising : Exn.t -> 'a -> 'b
   (**
    * Returns a function that raises the given exception when called.
    * {raising e} is equivalent to {let val e = e in fn _ => raise e end}.
    *)

   val recur : 'a -> ('a -> 'b) UnOp.t -> 'b
   (** {recur} is same as {Fn.flip Fn.fix}. *)

   val repeat : 'a UnOp.t -> Int.t -> 'a UnOp.t
   (** {repeat f n x} repeats {f} {n}-times starting with {x}. *)

   val undefined : 'a -> 'b
   (** An undefined function equivalent to {failing "undefined"}. *)
end
