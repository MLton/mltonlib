(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature PARSE_STRUCTS = 
   sig
      structure Javascript: JAVASCRIPT
      structure Token: TOKEN
      sharing Javascript.Regexp = Token.Regexp
   end

signature PARSE = 
   sig
      include PARSE_STRUCTS

      val parse: Token.t Stream.t -> Javascript.Program.t option
      val showConsider: bool ref
      val showAutoSemi: bool ref
   end
