(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported signatures == *)

signature WINDOWS    = WINDOWS
signature WINDOWS_EX = WINDOWS_EX

(** == Exported structures == *)

structure Windows : WINDOWS_EX = Windows
