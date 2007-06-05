(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature GENERIC = GENERIC
signature GENERICS = GENERICS
signature GENERIC_INDEX = GENERIC_INDEX
signature GENERIC_LIFTING = GENERIC_LIFTING
signature LIFTING = LIFTING

(** == Exported Structures == *)

structure Generics : GENERICS = Generics
structure Lifting : LIFTING = Lifting

(** == Exported Functors == *)

functor PairGenerics (Arg : sig
                         structure F : GENERIC
                         structure S : GENERIC
                      end) : GENERIC = PairGenerics (Arg)
