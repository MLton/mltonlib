(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Var = struct
   type 'a t = {get : 'a Thunk.t, set : 'a Effect.t}
   fun new v = let
      val r = ref v
   in
      {get = fn () => !r,
       set = fn v => r := v}
   end
end
