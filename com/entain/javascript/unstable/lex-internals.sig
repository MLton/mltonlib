(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature LEX_INTERNALS_STRUCTS =
   sig
      structure Source: SOURCE
      structure Token: TOKEN

      type arg
      type lexarg = {source: Source.t}
      type lexresult = Token.t
      type pos
      type yypos = int

      val addChar: char -> unit
      val addHexChar: string -> unit
      val addOctalChar: string -> unit
      val addRegexpChar: char -> unit
      val amAtStartOfLine: unit -> bool
      val eof: lexarg -> lexresult
      val error: Source.t * yypos * yypos * string -> unit
      val finishComment: unit -> unit
      val finishRegexp: yypos * {flags: string} -> Token.t
      val makeSlashDiv: unit -> unit
      val makeSlashNone: unit -> unit
      val makeSlashReg: unit -> unit
      val maybeFinishString: char * yypos -> Token.t option
      val newline: Source.t * yypos -> unit
      val regexpChars: char list ref
      val slashIsReg: unit -> bool
      val startComment: Source.t * yypos -> unit
      val startRegexp: Source.t * yypos -> unit
      val startString: Source.t * yypos * char -> unit
      val tok: Token.Variant.t * Source.t * yypos * yypos -> Token.t
   end
