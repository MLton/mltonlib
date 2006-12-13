(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature MONO_ARRAY = sig

   type t

   include GENERIC_ARRAY where type 'a t0 = t

end
