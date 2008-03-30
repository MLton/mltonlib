(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CPS :> CPS = struct
   open Fn CPS
   fun return x f = f x
   fun op >>= (aM, a2bM) = aM o flip a2bM
   type ('a, 'c) cont = 'a -> 'c
   fun callcc b k = b k k
   fun throw k x _ = k x
end
