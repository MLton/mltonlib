(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature USE_LIB = USE_LIB

(** == Exported Structures == *)

structure UseLib : USE_LIB = UseLib

(** == Exported Top-Level Values == *)

val lib = UseLib.lib
val use = UseLib.use
