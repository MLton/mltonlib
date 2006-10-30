(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature TOKEN_STRUCTS =
   sig
      structure Regexp: REGEXP
   end

signature TOKEN =
   sig
      include TOKEN_STRUCTS
      
      type t

      structure Variant:
         sig
            datatype t =
               ADDOP of string
             | ASSIGNOP of string
             | AUTO_SEMICOLON
             | BANG
             | BITOP of string
             | BOOL of bool
             | BREAK
             | CASE
             | CATCH
             | COLON
             | COMMA
             | CONST
             | CONTINUE
             | DEFAULT
             | DELETE
             | DO
             | DOT
             | ELSE
             | EOF
             | EQUALOP of string
             | EQUALS
             | FINALLY
             | FOR
             | FUNCTION
             | IDENTIFIER of string
             | IF
             | IN
             | INCOP of string
             | INSTANCE_OF
             | LBRACE
             | LBRACKET
             | LOGICOP of string
             | LPAREN
             | MULOP of string
             | NEW
             | NEWLINE_INCOP of string
             | NULL
             | NUMBER of real
             | QUESTION
             | RBRACE
             | RBRACKET
             | REGEXP of Regexp.t
             | RELOP of string
             | RETURN
             | RPAREN
             | SEMICOLON
             | SHIFTOP of string
             | STRING of Word.t vector
             | SWITCH
             | THIS
             | THROW
             | TILDE
             | TRY
             | TYPEOF
             | VAR
             | VOID
             | WHILE
             | WITH

            val equals: t * t -> bool
            val equalsIgnoreNewline: t * t -> bool
         end

      val isEof: t -> bool
      val left: t -> SourcePos.t
      val make: {left: SourcePos.t,
                 right: SourcePos.t,
                 startsLine: bool,
                 variant: Variant.t} -> t
      val mustSeparate: t * t -> bool
      val right: t-> SourcePos.t
      val startsLine: t -> bool
      val toString: t -> string
      val variant: t -> Variant.t
   end
