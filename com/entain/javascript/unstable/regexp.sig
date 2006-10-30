(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature REGEXP_STRUCTS =
   sig
   end

signature REGEXP =
   sig
      include REGEXP_STRUCTS

      type t

      val equals: t * t -> bool
      val make: {body: string, flags: string} -> t
      val toString: t -> string
      val layout: t -> Layout.t
   end
