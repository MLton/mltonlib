(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature PARSEC = PARSEC
signature SEQUENCE = SEQUENCE
signature STRING_SEQUENCE = STRING_SEQUENCE

(** == Exported Structures == *)

structure StringSequence : STRING_SEQUENCE = StringSequence

(** == Exported Functors == *)

signature MK_PARSEC_DOM = MK_PARSEC_DOM
functor MkParsec (Arg : MK_PARSEC_DOM) : PARSEC = MkParsec (Arg)
