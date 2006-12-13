(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Thunk: THUNK = struct

   type 'a t = Unit.t -> 'a

end

type 'a thunk = 'a Thunk.t
