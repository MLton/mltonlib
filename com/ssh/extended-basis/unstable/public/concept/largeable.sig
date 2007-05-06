(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Largeable ==
 *
 * Corresponding to each group of largeable types (e.g. Int and Word
 * types) there exists a "large" type (e.g. LargeInt, LargeWord) that can
 * represent any value of the group of largeable types.
 *)

signature LARGEABLE = sig
   type largeable
   type largeable_large
   val fromLarge : largeable_large -> largeable
   val isoLarge : (largeable, largeable_large) Iso.t
   val toLarge : largeable -> largeable_large
end

signature LARGEABLE_X = sig
   include LARGEABLE
   val isoLargeX : (largeable, LargeWord.t) Iso.t
   val toLargeX : largeable -> largeable_large
end
