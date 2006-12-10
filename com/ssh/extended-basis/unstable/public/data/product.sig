(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A general purpose product datatype.
 *
 * A product datatype is useful for reducing the inconvenience of matching
 * nested products in combinator libraries (e.g. parser combinators).
 *)
signature PRODUCT = sig
   datatype ('a, 'b) product = & of 'a * 'b

   include PRODUCT_TYPE
      where type ('a, 'b) t = ('a, 'b) product
end
