(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature LEX_STRUCTS =
   sig
      structure Source: SOURCE
      structure Token: TOKEN
   end

signature LEX =
   sig
      include LEX_STRUCTS

      val lexFile: File.t -> Token.t Stream.t
      val lexString: string -> Token.t Stream.t
      val showTokens: bool ref
   end
