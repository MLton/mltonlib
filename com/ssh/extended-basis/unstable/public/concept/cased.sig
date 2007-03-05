(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Cased == *)

signature CASED = sig
   type cased
   val toLower : cased UnOp.t
   val toUpper : cased UnOp.t
end
