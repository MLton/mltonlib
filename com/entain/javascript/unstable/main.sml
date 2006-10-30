(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Main: MAIN =
struct

local
   open Token
in
   structure Variant = Variant
end
   
fun makeOptions {usage = _} =
   let
      open Popt
   in
      List.map
      ([(Normal, "mozilla", " {false|true}",
         "accept mozilla extensions",
         boolRef Control.acceptMozillaExtensions),
        (Normal, "show-auto-semi", " {false|true}",
         "show automatic semicolon insertion",
         boolRef Parse.showAutoSemi),
        (Normal, "show-consider", " {false|true}",
         "show grammar expressions",
         boolRef Parse.showConsider),
        (Normal, "show-tokens", " {false|true}",
         "show tokens as the lexer gets them",
         boolRef Lex.showTokens),
        (Expert, #1 trace, " name1,...", "trace internals", #2 trace)],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end
   
val mainUsage = "mjs [option ...] {compress|parse|tokenize} file"
   
val {parse = parseOpts, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => false}

val usage = fn s => (usage s; raise Fail "unreachable")

fun tokenize f =
   Stream.foreach
   (Lex.lexFile f, fn t => print (concat [Token.toString t, "\n"]))

fun parse f =
   case Parse.parse (Lex.lexFile f) of
      NONE => Error.bug "parse failed"
    | SOME p => Layout.outputl (Javascript.Program.layout p, Out.standard)

fun compress f =
   case Parse.parse (Lex.lexFile f) of
      NONE => Error.bug "parse failed"
    | SOME p =>
         File.withTempOut
         (fn out => Layout.outputl (Javascript.Program.layout p, out),
          fn f =>
          let
             val tokens = Lex.lexFile f
          in
             case Stream.get tokens of
                NONE => ()
              | SOME (ts, t) =>
                   File.withTempOut
                   (fn out =>
                    let
                       val print = Out.outputc out
                    in
                       print (Token.toString t)
                       ; (ignore
                          (Stream.fold (ts, t, fn (t, t') =>
                                        let
                                           val () =
                                              if Token.mustSeparate (t', t)
                                                 then print " "
                                              else ()
                                           val () = print (Token.toString t)
                                        in
                                           t
                                        end)))
                    end,
                    fn f =>
                    let
                       val tokens' =
                          Lex.lexFile f
                          handle _ => Error.bug "broken output: not lexable"
                       val () = File.outputContents (f, Out.standard)
                    in
                       case (Stream.peek2
                             (tokens, tokens', fn (t, t') =>
                              not (Variant.equalsIgnoreNewline
                                   (Token.variant t, Token.variant t')))) of
                          NONE => ()
                        | SOME (t, t') =>
                             let
                                fun pt t =
                                   let
                                      val left = Token.left t
                                   in
                                      Out.output
                                      (Out.error,
                                       concat [SourcePos.toString left,
                                               " ", Token.toString t, "\n"])
                                   end
                                val () = pt t
                                val () = pt t'
                             in
                                Error.bug "broken output"
                             end
                    end)
          end)

fun commandLine (args: string list): unit =
   let
      val result = parseOpts args
   in
      case result of
         Result.No msg => usage msg
       | Result.Yes (command :: args) =>
            (case command of
                "compress" =>
                   (case args of
                       [file] => compress file
                     | _ => usage "compress must be supplied a file")
              | "parse" =>
                   (case args of
                       [file] => parse file
                     | _ => usage "parse must be supplied a file")
              | "tokenize" =>
                   (case args of
                       [file] => tokenize file
                     | _ => usage "tokenize must be supplied a file")
              | _ => usage (concat ["invalid command: ", command]))
       | _ => usage "must suply a command"
   end

val commandLine = Process.makeCommandLine commandLine

end

val () = OS.Process.exit (Main.commandLine (CommandLine.arguments ()))
