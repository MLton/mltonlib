(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature LAZY = sig

   val memo: 'a thunk -> 'a thunk
   (**
    * memo f returns a function g that, the first time it is called, calls f,
    * caches the value, and upon subsequent calls returns the cached value.
    *)

end
