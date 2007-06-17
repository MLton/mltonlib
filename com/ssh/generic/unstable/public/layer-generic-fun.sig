(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerGeneric} functor.
 *)
signature LAYER_GENERIC_DOM = sig
   structure Outer : OPEN_GENERIC
   structure Result : LAYERED_GENERIC_REP
   sharing Outer.Rep = Result.Outer
   include CLOSED_GENERIC
   sharing Rep = Result.Closed
end
