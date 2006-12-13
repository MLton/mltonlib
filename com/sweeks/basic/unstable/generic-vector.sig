(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature GENERIC_VECTOR = sig

   include SEQUENCE

   val make: Int.t -> {done: Unit.t -> 'a t0,
                     sub: Int.t -> 'a elem,
                     update: Int.t * 'a elem -> Unit.t}

   structure Unsafe:
      sig
         val sub: 'a t0 * Int.t -> 'a elem
      end

end


