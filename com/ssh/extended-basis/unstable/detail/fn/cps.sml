(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CPS :> CPS = struct
   open CPS
   fun return x f = f x
   fun op >>= (aM, a2bM) = aM o Fn.flip a2bM
end
