(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for generic type representation expression.
 *)
signature TYPE_EXP = sig
   structure TypeExp : OPEN_REP

   val ty : ('a, 'x) TypeExp.t -> Ty.Var.t Ty.t
   (** Returns the type expression given a type representation. *)
end

signature TYPE_EXP_CASES = sig
   include OPEN_CASES TYPE_EXP
   sharing Rep = TypeExp
end
