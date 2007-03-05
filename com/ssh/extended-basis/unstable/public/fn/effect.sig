(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with side-effecting procedures. *)
signature EFFECT = sig
   type 'a t = 'a -> Unit.t
   (** Type of side-effecting procedures. *)

   val ignore : 'a t
   (** No-operation ({ignore = fn _ => ()}). *)

   val nop : Unit.t t
   (** No-operation ({nop = fn () => ()}). *)

   val obs : 'a t -> 'a UnOp.t
   (**
    * Side-effecting I-combinator ({obs ef x = (ef x ; x)}).  Using {obs}
    * and {o} you can "attach" side-effects to a function.  The name {obs}
    * is short for {observe} and comes from the idea that the data is
    * observed by the effect.
    *)

   val past : Unit.t t -> 'a UnOp.t
   (**
    * Side-effecting I-combinator ({past ef x = (ef () ; x)}).  Using
    * {past} and {o} you can "attach" side-effects to a function.  The
    * name {past} comes from the idea that the data flows past the effect.
    *)
end
