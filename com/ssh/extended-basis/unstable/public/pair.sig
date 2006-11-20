(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PAIR = sig
   type ('a, 'b) pair = 'a * 'b

   include PRODUCT_TYPE
      where type ('a, 'b) t = ('a, 'b) pair
end
