(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for generic type representation expression.
 *)
signature TYPE_EXP = sig
   structure TypeExpRep : OPEN_REP

   (** A minimalistic type variable representation providing only equality. *)
   structure TypeVar : sig
      eqtype t
   end

   val ty : ('a, 'x) TypeExpRep.t -> TypeVar.t Ty.t
   (** Returns the type expression given a type representation. *)
end

signature TYPE_EXP_CASES = sig
   include CASES TYPE_EXP
   sharing Open.Rep = TypeExpRep
end

signature WITH_TYPE_EXP_DOM = CASES
