(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature PARSEC = PARSEC
signature SEQUENCE = SEQUENCE
signature VECTOR_SEQUENCE = VECTOR_SEQUENCE

(** == Exported Structures == *)

structure StringSequence : VECTOR_SEQUENCE = StringSequence
structure Word8VectorSequence : VECTOR_SEQUENCE = Word8VectorSequence

(** == Exported Functors == *)

signature MK_PARSEC_DOM = MK_PARSEC_DOM
functor MkParsec (Arg : MK_PARSEC_DOM) : PARSEC = MkParsec (Arg)
