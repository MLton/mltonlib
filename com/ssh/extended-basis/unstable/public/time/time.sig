(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Extended {TIME} signature.
 *)
signature TIME = sig
   include BASIS_TIME
   type t = time
   val fromHours : LargeInt.t -> t
   val fromMinutes : LargeInt.t -> t
   val toHours : t -> LargeInt.t
   val toMinutes : t -> LargeInt.t
end
