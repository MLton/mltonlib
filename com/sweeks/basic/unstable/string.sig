(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature STRING = sig

   include MONO_VECTOR where type 'a elem = Char.t

   val hasPrefix: t * t -> Bool.t
   val ofWord8Vector: Word8.t Vector.t -> t
   val toLower: t -> t
   val toUpper: t -> t
   val toWord8Vector: t -> Word8.t Vector.t

end
