(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature PACKABLE_WORD = sig

   include WORD

   val subArr: Word8.t Array.t * Int.t * Endian.t -> t
   val subVec: Word8.t Vector.t * Int.t * Endian.t -> t
   val update: Word8.t Array.t * Int.t * t * Endian.t -> Unit.t
      
end
