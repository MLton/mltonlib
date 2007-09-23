(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerCases} functor.
 *)
signature LAYER_CASES_DOM = sig
   structure Open : OPEN_CASES
   include LAYERED_REP CLOSED_CASES
   sharing Open.Rep = Outer
   sharing Rep = This
end
