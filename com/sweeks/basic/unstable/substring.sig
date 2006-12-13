(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature SUBSTRING = sig

   include MONO_VECTOR_SLICE where type 'a elem = Char.t

end
