(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Lex (S: LEX_STRUCTS): LEX =
struct

open S

val showTokens = ref false
   
local
   open Control
in
   structure Region = Region
end
local
   open Region
in
   structure SourcePos = SourcePos
end
local
   open Token
in
   structure Variant = Variant
end

structure LexInternalsArg =
   struct
      structure Source = Source
      structure Token = Token

      type yypos = int

      type pos = SourcePos.t
      type lexresult = Token.t
      type lexarg = {source: Source.t}
      type arg = lexarg

      fun error (source, left, right, msg) = 
         Control.errorStr (Region.make {left = Source.getPos (source, left),
                                        right = Source.getPos (source, right)},
                           msg)

      structure SlashState =
         struct
            datatype t =
               Div
             | None
             | Regexp
         end

      local
         datatype z = datatype SlashState.t
         val slashState = ref Regexp
      in
         val makeSlashReg: unit -> unit =
            fn () => slashState := Regexp

         val makeSlashDiv: unit -> unit =
            fn () => slashState := Div
      
         val makeSlashNone: unit -> unit =
            fn () => slashState := None
      
         val slashIsReg: unit -> bool =
            fn () =>
            case !slashState of
               Div => false
             | None => Error.bug "unexpected slash"
             | Regexp => true
      end

      val atStartOfLine = ref true

      fun amAtStartOfLine () = !atStartOfLine

      fun newline z =
         (atStartOfLine := true
          ; Source.newline z)
    
      fun tok (t, s: Source.t, l: int, r: int) =
         let
            val startsLine = !atStartOfLine
            val () = atStartOfLine := false
            fun pos p = Source.getPos (s, p)
         in
            Token.make {left = pos l,
                        right = pos r,
                        startsLine = startsLine,
                        variant = t}
         end

      val commentStart: {startPos: SourcePos.t} option ref = ref NONE

      fun startComment (source, yypos) =
         commentStart := SOME {startPos = Source.getPos (source, yypos)}

      fun finishComment () = commentStart := NONE

      val regexpChars: char list ref = ref []

      fun addRegexpChar c = List.push (regexpChars, c)

      val regexpStart: {source: Source.t,
                        startPos: SourcePos.t,
                        yypos: int} option ref = ref NONE

      fun startRegexp (source, yypos) =
         regexpStart := SOME {source = source,
                              startPos = Source.getPos (source, yypos),
                              yypos = yypos}

      fun finishRegexp (finish, {flags}) =
         let
            val body = implode (rev (!regexpChars))
            val {source, yypos = start, ...} = valOf (!regexpStart)
            val () = regexpStart := NONE
            val () = regexpChars := []
         in
            tok (Variant.REGEXP (Token.Regexp.make
                                 {body = body, flags = flags}),
                 source, start, finish)
         end

      val stringChars: Word.t list ref = ref []

      val stringStart: {source: Source.t,
                        startChar: char,
                        startPos: SourcePos.t,
                        yypos: int} option ref = ref NONE

      fun addChar c = List.push (stringChars, Word.fromInt (Char.toInt c))

      fun startString (source, yypos, c) =
         stringStart := SOME {source = source,
                              startChar = c,
                              startPos = Source.getPos (source, yypos),
                              yypos = yypos}

      fun maybeFinishString (c, finishPos) =
         let
            val {source, startChar, yypos, ...} = valOf (!stringStart)
         in
            if c <> startChar
               then (addChar c; NONE)
            else
               let
                  val s = Vector.fromListRev (! stringChars)
                  val () = stringChars := []
                  val () = stringStart := NONE
               in
                  SOME (tok (Variant.STRING s, source, yypos, finishPos))
               end
         end

      fun addOctalChar yytext =
         List.push
         (stringChars,
          String.fold (String.dropPrefix (yytext, 1), 0w0, fn (c, w) =>
                       Word.<< (w, 0w3)
                       + Word.fromInt (Char.ord c - Char.ord #"0")))
         
      fun addHexChar yytext =
         List.push
         (stringChars,
          String.fold (String.dropPrefix (yytext, 2), 0w0, fn (c, w) =>
                       Word.<< (w, 0w4) + Word.fromInt (Char.toHexDigit c)))

      val eof: lexarg -> lexresult =
         fn {source, ...} =>
         let
            val pos = Source.lineStart source
            fun check (start, msg, f) =
               case !start of
                  NONE => ()
                | SOME s =>
                     Control.errorStr (Region.make {left = f s, right = pos},
                                       (concat ["end of file inside ", msg]))
            val () = check (commentStart, "comment", #startPos)
            val () = check (regexpStart, "regexp", #startPos)
            val () = check (stringStart, "string", #startPos)
         in
            Token.make {left = pos,
                        right = pos,
                        startsLine = !atStartOfLine,
                        variant = Variant.EOF}
         end
   end

structure Lex = LexInternals (LexInternalsArg)

structure LexInput =
   struct
      datatype t = T of {inputN: int -> string,
                         name: string}
   end

fun lex (LexInput.T {inputN, name})= 
   let
      val get = Lex.makeLexer inputN {source = Source.new name}
      fun loop () =
         Stream.memo
         (fn () =>
          let
             val t = get ()
             val () =
                if !showTokens then
                   Out.outputl
                   (Out.error,
                    concat ["got ", Token.toString t,
                            " at ", SourcePos.toString (Token.left t)])
                else
                   ()
          in
             if Token.isEof t then
                NONE
             else
                let
                   val rest = loop ()
                   fun normal () = SOME (rest, t)
                   fun insertSemi () =
                      let
                         val pos = Token.left t
                         val semi =
                            Token.make {left = pos,
                                        right = pos,
                                        startsLine = false,
                                        variant = Variant.AUTO_SEMICOLON}
                      in
                         SOME (Stream.prefix (rest, semi), t)
                      end
                in
                   if let
                         datatype z = datatype Variant.t
                      in
                         case Token.variant t of
                            BREAK => true
                          | CONTINUE => true
                          | RETURN => true
(* Section 7.9.1 of the ECMAScript spec says to automatically insert a
 * semicolon after a throw if it is followed by a newline.  However, this is
 * inconsistent with the fact that throw requires an expression, while break,
 * continue, and return do not.  Also, at least Firefox doesn't do the semicolon
 * insertion after throw.
 *)
(*                        | THROW => true *)
                          | _ => false
                      end
                      then (case Stream.get rest of
                               NONE => insertSemi ()
                             | SOME (_, t) =>
                                  if Token.startsLine t
                                     then insertSemi ()
                                  else normal ())
                   else normal ()
                end
          end)
   in
      loop ()
   end

fun lexFile (f: File.t): Token.t Stream.t =
   let
      val ins = In.openIn f
   in
      lex (LexInput.T {inputN = fn n => In.inputN (ins, n),
                       name = f})
   end

fun lexString s =
   let
      val m = String.size s
      val r = ref 0
      fun inputN n =
         let
            val start = !r
            val finish = Int.min (start + n, m)
            val () = r := finish
         in
            String.substring2 (s, {start = start, finish = finish})
         end
   in
      lex (LexInput.T {inputN = inputN,
                       name = "<string>"})
   end

end
