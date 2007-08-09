(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerRep} functor.
 *)
signature LAYER_REP_DOM = sig
   structure Outer : OPEN_REP
   structure Closed : CLOSED_REP
end
