(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Intable ==
 *
 * Intables can be converted to integers and back.
 *)

signature INTABLE = sig
   type intable
   val fromInt : Int.t -> intable
   val fromLargeInt : LargeInt.t -> intable
   val isoInt : (intable, Int.t) Iso.t
   val isoLargeInt : (intable, LargeInt.t) Iso.t
   val toInt : intable -> Int.t
   val toLargeInt : intable -> LargeInt.t
end

signature INTABLE_X = sig
   include INTABLE
   val isoIntX : (intable, Int.t) Iso.t
   val isoLargeIntX : (intable, LargeInt.t) Iso.t
   val toIntX : intable -> Int.t
   val toLargeIntX : intable -> LargeInt.t
end
