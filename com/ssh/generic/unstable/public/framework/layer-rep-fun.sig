(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {LayerRep} functor.
 *)
signature LAYER_REP_DOM = sig
   structure Open : sig
      structure Rep : OPEN_REP
   end
   include CLOSED_REP
end

(**
 * Signature for the domain of the {LayerRep'} functor.
 *)
signature LAYER_REP_DOM' = sig
   structure Open : sig
      structure Rep : OPEN_REP
   end
   type 'a t (** Type of representations. *)
end

(**
 * Signature for the codomain of the {LayerRep} functor.
 *)
signature LAYER_REP_COD = sig
   include LAYERED_REP
   structure Rep : CLOSED_REP
   sharing Rep = This
end
