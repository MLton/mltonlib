(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature VECTOR = sig

   type 'a t

   include GENERIC_VECTOR where type 'a t0 = 'a t
      
end
