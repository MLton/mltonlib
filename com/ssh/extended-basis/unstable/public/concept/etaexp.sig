(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Eta-expandable ==
 *
 * Values of an eta-expandable type can be eta-expanded.
 * [http://mlton.org/EtaExpansion Eta-expansion] can be used as a
 * workaround for the [http://mlton.org/ValueRestriction value
 * restriction] in Standard ML.
 *
 * The idea is that the type constructors {etaexp_dom} and {etaexp_cod}
 * are used to expose the arrow {->} type constructor (to allow
 * eta-expansion) while keeping the domain and codomain abstract.
 *)

(** Eta-expandable with a type parameter. *)
signature ETAEXP' = sig
   type 'a etaexp_dom and 'a etaexp_cod
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod
end
