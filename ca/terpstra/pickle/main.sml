fun prerr s =
  TextIO.outputSubstr (TextIO.stdErr, Substring.full s)
fun help () = (
  prerr "Usage: pickle {functor-name} < {input-file} > {output-file}\n";
  OS.Process.exit OS.Process.failure)

structure LrVals = TMLLrValsFun(structure Token = LrParser.Token)
structure Lex = TMLLexFun(structure Tokens = LrVals.Tokens)
structure Parse = Join(structure ParserData = LrVals.ParserData
                       structure Lex = Lex
                       structure LrParser = LrParser)

val name = 
  case CommandLine.arguments () of 
      (x :: []) => x
    | (x :: r) => ""
    | [] => ""
val () = if name = "" then help () else ()

fun signame s =
  let
    fun chr (i, c, t) =
      if i <> 0 andalso Char.isUpper c andalso
         not (Char.isUpper (String.sub (s, i - 1)))
      then #"_" :: Char.toUpper c :: t
      else Char.toUpper c :: t
  in
    implode (CharVector.foldri chr [] s)
  end

val argname = signame name ^ "_ARG"
val signame = signame name

fun error (s, (), ()) = print ("Error: " ^ s ^ "\n")
fun reader _ = 
  case TextIO.inputLine TextIO.stdIn of
      SOME x => x
    | NONE => ""

val stream = Parse.makeLexer reader
val lookahead = 30
val ast = #1 (Parse.parse (lookahead, stream, error, ()))
val (start, ast) = tag ast

fun dump l = List.app print (List.rev l)
fun rungen f = #1 (List.foldl f ([], Map.empty) ast)

val () = (
  print ("signature " ^ argname ^ " =\n");
  print ("  sig\n");
  print ("    structure Base : SERIAL_BASE\n");
  dump (rungen import);
  print ("  end\n\n");
  print ("signature " ^ signame ^ " =\n");
  print ("  sig\n");
  print ("    structure Base : SERIAL_BASE\n");
  print ("    structure Arg : " ^ argname ^ "\n");
  dump (rungen export);
  print ("  end\n\n");
  print ("functor " ^ name ^ "(Arg : " ^ argname ^ ") : " ^ signame ^ " =\n");
  print ("  struct\n");
  print ("    structure Base = Arg.Base\n");
  print ("    structure Arg = Arg\n");
  print ("    exception Corrupt\n");
  dump (rungen (method start));
  print ("  end\n"))
