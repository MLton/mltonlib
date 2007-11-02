(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature CAST_REAL = sig
   type t
   structure Bits : WORD
   val isoBits : (t, Bits.t) Iso.t Option.t
end
