(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Iso : ISO = struct
   open Iso

   infix <-->

   val swap = Pair.swap

   fun (a2b, b2a) <--> (c2a, a2c) = (a2b o c2a, a2c o b2a)

   fun map (l, r) iso = r <--> iso <--> l

   local
      fun mk map = Pair.map map o Pair.swizzle
   in
      fun op --> ? = mk (Fn.map, Fn.map) ?
      fun op  +` ? = mk (Sum.map, Sum.map) ?
      fun op  *` ? = mk (Product.map, Product.map) ?
   end
end
