(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Token (S: TOKEN_STRUCTS): TOKEN =
struct

open S

structure Variant =
   struct
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

      val isEof =
         fn EOF => true
          | _ => false

      val toString =
         fn ADDOP s => s
          | ASSIGNOP s => s
          | AUTO_SEMICOLON => ";"
          | BANG => "!"
          | BITOP s => s
          | BOOL b => if b then "true" else "false"
          | BREAK => "break"
          | CASE => "case"
          | CATCH => "catch"
          | COLON => ":"
          | COMMA => ","
          | CONST => "const"
          | CONTINUE => "continue"
          | DEFAULT => "default"
          | DELETE => "delete"
          | DO => "do"
          | DOT => "."
          | ELSE => "else"
          | EOF => "<eof>"
          | EQUALOP s => s
          | EQUALS => "="
          | FINALLY => "finally"
          | FOR => "for"
          | FUNCTION => "function"
          | IDENTIFIER s => s
          | IF => "if"
          | IN => "in"
          | INCOP s => s
          | INSTANCE_OF => "instanceof"
          | LBRACE => "{"
          | LBRACKET => "["
          | LOGICOP s => s
          | LPAREN => "("
          | MULOP s => s
          | NEW => "new"
          | NEWLINE_INCOP s => s
          | NULL => "null"
          | NUMBER r => Util.realToJavascript r
          | QUESTION => "?"
          | RBRACE => "}"
          | RBRACKET => "]"
          | REGEXP r => Regexp.toString r
          | RELOP s => s
          | RETURN => "return"
          | RPAREN => ")"
          | SEMICOLON => ";"
          | SHIFTOP s => s
          | STRING s => Util.escapeJavascript s
          | SWITCH => "switch"
          | THIS => "this"
          | THROW => "throw"
          | TILDE => "~"
          | TRY => "try"
          | TYPEOF => "typeof"
          | VAR => "var"
          | VOID => "void"
          | WHILE => "while"
          | WITH => "with"

      val layout = Layout.str o toString

      val equals =
         fn (ADDOP s, ADDOP s') => s = s'
          | (ASSIGNOP s, ASSIGNOP s') => s = s'
          | (AUTO_SEMICOLON, AUTO_SEMICOLON) => true
          | (BANG, BANG) => true
          | (BITOP s, BITOP s') => s = s'
          | (BOOL b, BOOL b') => b = b'
          | (BREAK, BREAK) => true
          | (CASE, CASE) => true
          | (CATCH, CATCH) => true
          | (COLON, COLON) => true
          | (COMMA, COMMA) => true
          | (CONST, CONST) => true
          | (CONTINUE, CONTINUE) => true
          | (DEFAULT, DEFAULT) => true
          | (DELETE, DELETE) => true
          | (DO, DO) => true
          | (DOT, DOT) => true
          | (ELSE, ELSE) => true
          | (EOF, EOF) => true
          | (EQUALOP s, EQUALOP s') => s = s'
          | (EQUALS, EQUALS) => true
          | (FINALLY, FINALLY) => true
          | (FOR, FOR) => true
          | (FUNCTION, FUNCTION) => true
          | (IDENTIFIER s, IDENTIFIER s') => s = s'
          | (IF, IF) => true
          | (IN, IN) => true
          | (INCOP s, INCOP s') => s = s'
          | (INSTANCE_OF, INSTANCE_OF) => true
          | (LBRACE, LBRACE) => true
          | (LBRACKET, LBRACKET) => true
          | (LOGICOP s, LOGICOP s') => s = s'
          | (LPAREN, LPAREN) => true
          | (MULOP s, MULOP s') => s = s'
          | (NEW, NEW) => true
          | (NEWLINE_INCOP s, NEWLINE_INCOP s') => s = s'
          | (NULL, NULL) => true
          | (NUMBER r, NUMBER r') => Real.equals (r, r')
          | (QUESTION, QUESTION) => true
          | (RBRACE, RBRACE) => true
          | (RBRACKET, RBRACKET) => true
          | (REGEXP r, REGEXP r') => Regexp.equals (r, r')
          | (RELOP s, RELOP s') => s = s'
          | (RETURN, RETURN) => true
          | (RPAREN, RPAREN) => true
          | (SEMICOLON, SEMICOLON) => true
          | (SHIFTOP s, SHIFTOP s') => s = s'
          | (STRING s, STRING s') => s = s'
          | (SWITCH, SWITCH) => true
          | (THIS, THIS) => true
          | (THROW, THROW) => true
          | (TILDE, TILDE) => true
          | (TRY, TRY) => true
          | (TYPEOF, TYPEOF) => true
          | (VAR, VAR) => true
          | (VOID, VOID) => true
          | (WHILE, WHILE) => true
          | (WITH, WITH) => true
          | _ => false

      val equalsIgnoreNewline =
         fn (v, v') =>
         equals (v, v')
         orelse
         case (v, v') of
            (INCOP _, NEWLINE_INCOP _) => true
          | (NEWLINE_INCOP _, INCOP _) => true
          | _ => false

      structure Kind =
         struct
            datatype t =
               Alphabetic
             | Eof
             | Number
             | Regexp
             | String
             | Symbolic
         end
      
      fun kind v =
         let
            datatype z = datatype Kind.t
         in
            case v of
               BOOL _ => Alphabetic
             | ADDOP _ => Symbolic
             | ASSIGNOP _ => Symbolic
             | AUTO_SEMICOLON => Symbolic
             | BANG => Symbolic
             | BITOP _ => Symbolic
             | BREAK => Alphabetic
             | CASE => Alphabetic
             | CATCH => Alphabetic
             | COLON => Symbolic
             | COMMA => Symbolic
             | CONST => Alphabetic
             | CONTINUE => Alphabetic
             | DEFAULT => Alphabetic
             | DELETE => Alphabetic
             | DO => Alphabetic
             | DOT => Symbolic
             | ELSE => Alphabetic
             | EOF => Eof
             | EQUALOP _ => Symbolic
             | EQUALS => Symbolic
             | FINALLY => Alphabetic
             | FOR => Alphabetic
             | FUNCTION => Alphabetic
             | IDENTIFIER _ => Alphabetic
             | IF => Alphabetic
             | IN => Alphabetic
             | INCOP _ => Symbolic
             | INSTANCE_OF => Alphabetic
             | LBRACE => Symbolic
             | LBRACKET => Symbolic
             | LOGICOP _ => Symbolic
             | LPAREN => Symbolic
             | MULOP _ => Symbolic
             | NEW => Alphabetic
             | NEWLINE_INCOP _ => Symbolic
             | NULL => Alphabetic
             | NUMBER _ => Number
             | QUESTION => Symbolic
             | RBRACE => Symbolic
             | RBRACKET => Symbolic
             | REGEXP _ => Regexp
             | RELOP _ => Symbolic
             | RETURN => Alphabetic
             | RPAREN => Symbolic
             | SEMICOLON => Symbolic
             | SHIFTOP _ => Symbolic
             | STRING _ => String
             | SWITCH => Alphabetic
             | THIS => Alphabetic
             | THROW => Alphabetic
             | TILDE => Symbolic
             | TRY => Alphabetic
             | TYPEOF => Alphabetic
             | VAR => Alphabetic
             | VOID => Alphabetic
             | WHILE => Alphabetic
             | WITH => Alphabetic
         end

      fun mustSeparate (v1, v2) =
         let
            datatype z = datatype Kind.t
         in
            case (kind v1, kind v2) of
               (Alphabetic, Alphabetic) => true
             | (Alphabetic, Number) => true
             | (Number, Symbolic) =>
                  (case (v1, v2) of
                      (NUMBER _, DOT) => true
                    | _ => false)
             | (Regexp, Alphabetic) => true
             | (Regexp, Number) => true
             | (Symbolic, Symbolic) =>
                  (case (v1, v2) of
                      (ADDOP _, ADDOP _) => true
                    | _ => false)
             | _ => false
         end

      val mustSeparate =
         Trace.trace2 ("mustSeparate", layout, layout, Bool.layout)
         mustSeparate
   end

datatype t = T of {left: SourcePos.t,
                   right: SourcePos.t,
                   startsLine: bool,
                   variant: Variant.t}

val make = T

local
   fun make f (T r) = f r
in
   val left = make #left
   val right = make #right
   val startsLine = make #startsLine
   val variant = make #variant
end
   
val isEof = Variant.isEof o variant
val toString: t -> string = Variant.toString o variant

fun mustSeparate (t1, t2) = Variant.mustSeparate (variant t1, variant t2)

end
