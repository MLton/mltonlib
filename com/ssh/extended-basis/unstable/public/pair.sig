(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature PAIR = sig
   type ('a, 'b) pair = 'a * 'b

   include PRODUCT_TYPE
      where type ('a, 'b) t = ('a, 'b) pair
end
