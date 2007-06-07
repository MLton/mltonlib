(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature GENERICS = GENERICS

signature GENERIC = GENERIC
signature GENERIC_INDEX = GENERIC_INDEX

signature EXT_GENERIC = EXT_GENERIC
signature EXT_GENERIC_INDEX = EXT_GENERIC_INDEX

signature EXT_GENERIC_COD = EXT_GENERIC_COD
signature EXT_GENERIC_DOM = EXT_GENERIC_DOM

(** == Exported Structures == *)

structure Generic : EXT_GENERIC = Generic
structure Generics : GENERICS = Generics

(** == Exported Functors == *)

functor ExtGeneric (Arg : EXT_GENERIC_DOM) : EXT_GENERIC_COD = ExtGeneric (Arg)
