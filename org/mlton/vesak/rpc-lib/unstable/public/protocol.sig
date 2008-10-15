(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature PROTOCOL = sig
   structure Signature : sig
      type ('d, 'c) t = 'd Rep.t * 'c Rep.t * String.t
   end

   structure Fingerprint : sig
      eqtype t
      val toString : t -> String.t
      val fromSignature : ('d, 'c) Signature.t -> t
   end

   structure Version : sig
      eqtype t
      val toString : t -> String.t
      val current : t
   end
end
