(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
functor EnumerateGet (S: GET): ENUMERATE = struct

   open S
   type 'a const = Unit.t
   type 'a state = 'a t
   fun start s = ((), s)
   fun next ((), s) = get s
      
end
