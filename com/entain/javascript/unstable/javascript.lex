(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open S
structure T = Token.Variant
val size = String.size
   
%%
%s MLC R S SLC;
%header (functor LexInternals (S: LEX_INTERNALS_STRUCTS));
%arg (arg as {source});

IdentifierPart=[a-zA-Z_$0-9];
Identifier=[a-zA-Z_$]{IdentifierPart}*;
WhiteSpace=("\012"|[\t\ ]);
string="'"|"\"";

DecimalDigit=[0-9];
NonZeroDigit=[1-9];
HexDigit=[0-9a-fA-F];
ExponentPart=[eE][+-]?{DecimalDigit}+;
HexIntegerLiteral="0"[xX]{HexDigit}+;
DecimalIntegerLiteral={DecimalDigit}+;
DecimalLiteral={DecimalIntegerLiteral}"."{DecimalDigit}*{ExponentPart}?|"."{DecimalDigit}+{ExponentPart}?|{DecimalIntegerLiteral}{ExponentPart}?;
OctalDigit=[0-7];
OctalLiteral="0"{OctalDigit}+;


OctalEscape={OctalDigit}{1,2}|[0-3]{OctalDigit}{2};

LineTerminator="\010"|"\013\010";

RegularExpressionFlags={IdentifierPart}*;

Quote="\""|"'";

%%

<INITIAL>{WhiteSpace} => (continue ());
<INITIAL>"//" => (YYBEGIN SLC; continue ());
<INITIAL>"/*" => (startComment (source, yypos); YYBEGIN MLC; continue ());
<INITIAL>{LineTerminator} =>
   (newline (source, yypos + size yytext - 1); continue ());

<SLC>{LineTerminator} =>
   (newline (source, yypos + size yytext - 1); YYBEGIN INITIAL; continue ());
<SLC>. => (continue ());

<MLC>"*/" => (finishComment (); YYBEGIN INITIAL; continue ());
<MLC>{LineTerminator} =>
   (newline (source, yypos + size yytext - 1) ; continue ());
<MLC>. => (continue ());
   
<INITIAL>"-"|"+" =>
   (makeSlashReg ()
    ; tok (T.ADDOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"-="|"*="|"%="|"+="|"&="|"^="|"|="|"<<="|">>="|">>>=" =>
   (makeSlashReg ()
    ; tok (T.ASSIGNOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"!" =>
   (makeSlashReg ()
    ; tok (T.BANG, source, yypos, yypos + size yytext));
<INITIAL>"|"|"&"|"^" =>
   (makeSlashReg ()
    ; tok (T.BITOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"break" =>
   (makeSlashNone ()
    ; tok (T.BREAK, source, yypos, yypos + size yytext));
<INITIAL>"case" =>
   (makeSlashNone ()
    ; tok (T.CASE, source, yypos, yypos + size yytext));
<INITIAL>"catch" =>
   (makeSlashNone ()
    ; tok (T.CATCH, source, yypos, yypos + size yytext));
<INITIAL>":" =>
   (makeSlashReg ()
    ; tok (T.COLON, source, yypos, yypos + size yytext));
<INITIAL>"," =>
   (makeSlashReg ()
    ; tok (T.COMMA, source, yypos, yypos + size yytext));
<INITIAL>"continue" =>
   (makeSlashNone ()
    ; tok (T.CONTINUE, source, yypos, yypos + size yytext));
<INITIAL>"const" =>
   (if !Control.acceptMozillaExtensions
       then (makeSlashNone ()
             ; tok (T.CONST, source, yypos, yypos + size yytext))
    else (makeSlashDiv ()
          ; tok (T.IDENTIFIER yytext, source, yypos, yypos + size yytext)));
<INITIAL>"default" =>
   (makeSlashNone ()
    ; tok (T.DEFAULT, source, yypos, yypos + size yytext));
<INITIAL>"delete" =>
   (makeSlashReg ()
    ; tok (T.DELETE, source, yypos, yypos + size yytext));
<INITIAL>"do" =>
   (makeSlashReg ()
    ; tok (T.DO, source, yypos, yypos + size yytext));
<INITIAL>"." =>
   (makeSlashNone ()
    ; tok (T.DOT, source, yypos, yypos + size yytext));
<INITIAL>"else" =>
   (makeSlashReg ()
    ; tok (T.ELSE, source, yypos, yypos + size yytext));
<INITIAL>"=="|"!="|"==="|"!==" =>
   (makeSlashReg ()
    ; tok (T.EQUALOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"=" =>
   (makeSlashReg ()
    ; tok (T.EQUALS, source, yypos, yypos + size yytext));
<INITIAL>"false" =>
   (makeSlashDiv ()
    ; tok (T.BOOL false, source, yypos, yypos + size yytext));
<INITIAL>"finally" =>
   (makeSlashNone ()
    ; tok (T.FINALLY, source, yypos, yypos + size yytext));
<INITIAL>"for" =>
   (makeSlashNone ()
    ; tok (T.FOR, source, yypos, yypos + size yytext));
<INITIAL>"function" =>
   (makeSlashNone ()
    ; tok (T.FUNCTION, source, yypos, yypos + size yytext));
<INITIAL>"if" =>
   (makeSlashNone ()
    ; tok (T.IF, source, yypos, yypos + size yytext));
<INITIAL>"in" =>
   (makeSlashReg ()
    ; tok (T.IN, source, yypos, yypos + size yytext));
<INITIAL>"++"|"--" =>
   (makeSlashDiv ()
    ; tok (if amAtStartOfLine ()
              then T.NEWLINE_INCOP yytext
           else T.INCOP yytext,
           source, yypos, yypos + size yytext));
<INITIAL>"instanceof" =>
   (makeSlashNone ()
    ; tok (T.INSTANCE_OF, source, yypos, yypos + size yytext));
<INITIAL>"{" =>
   (makeSlashReg ()
    ; tok (T.LBRACE, source, yypos, yypos + size yytext));
<INITIAL>"[" =>
   (makeSlashReg ()
    ; tok (T.LBRACKET, source, yypos, yypos + size yytext));
<INITIAL>"||"|"&&" =>
   (makeSlashReg ()
    ; tok (T.LOGICOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"(" =>
   (makeSlashReg ()
    ; tok (T.LPAREN, source, yypos, yypos + size yytext));
<INITIAL>"%"|"*" =>
   (makeSlashReg ()
    ; tok (T.MULOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"new" =>
   (makeSlashReg ()
    ; tok (T.NEW, source, yypos, yypos + size yytext));
<INITIAL>"null" =>
   (makeSlashReg ()
    ; tok (T.NULL, source, yypos, yypos + size yytext));
<INITIAL>{DecimalLiteral} =>
   (makeSlashDiv ()
    ; tok (T.NUMBER (valOf (Real.fromString yytext)),
           source, yypos, yypos + size yytext));
<INITIAL>{HexIntegerLiteral} =>
   (makeSlashDiv ()
    ; tok (T.NUMBER (Real.fromIntInf
                     (valOf (StringCvt.scanString
                             (fn r => IntInf.scan (StringCvt.HEX, r))
                             (String.dropPrefix (yytext, 2))))),
           source, yypos, yypos + size yytext));
<INITIAL>{OctalLiteral} =>
   (makeSlashDiv ()
    ; tok (T.NUMBER (Real.fromIntInf
                     (valOf (StringCvt.scanString
                             (fn r => IntInf.scan (StringCvt.OCT, r))
                             (String.dropPrefix (yytext, 1))))),
           source, yypos, yypos + size yytext));
<INITIAL>"?" =>
   (makeSlashReg ()
    ; tok (T.QUESTION, source, yypos, yypos + size yytext));
<INITIAL>"}" =>
   (makeSlashReg ()
    ; tok (T.RBRACE, source, yypos, yypos + size yytext));
<INITIAL>"]" =>
   (makeSlashDiv ()
    ; tok (T.RBRACKET, source, yypos, yypos + size yytext));
<INITIAL>"<"|">"|"<="|">=" =>
   (makeSlashReg ()
    ; tok (T.RELOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"return" =>
   (makeSlashReg ()
    ; tok (T.RETURN, source, yypos, yypos + size yytext));
<INITIAL>")" =>
   (makeSlashDiv ()
    ; tok (T.RPAREN, source, yypos, yypos + size yytext));
<INITIAL>";" =>
   (makeSlashReg ()
    ; tok (T.SEMICOLON, source, yypos, yypos + size yytext));
<INITIAL>"<<"|">>"|">>>" =>
   (makeSlashReg ()
    ; tok (T.SHIFTOP yytext, source, yypos, yypos + size yytext));
<INITIAL>"switch" =>
   (makeSlashNone ()
    ; tok (T.SWITCH, source, yypos, yypos + size yytext));
<INITIAL>"this" =>
   (makeSlashDiv ()
    ; tok (T.THIS, source, yypos, yypos + size yytext));
<INITIAL>"throw" =>
   (makeSlashReg ()
    ; tok (T.THROW, source, yypos, yypos + size yytext));
<INITIAL>"~" =>
   (makeSlashReg ()
    ; tok (T.TILDE, source, yypos, yypos + size yytext));
<INITIAL>"true" =>
   (makeSlashDiv ()
    ; tok (T.BOOL true, source, yypos, yypos + size yytext));
<INITIAL>"try" =>
   (makeSlashNone ()
    ; tok (T.TRY, source, yypos, yypos + size yytext));
<INITIAL>"typeof" =>
   (makeSlashReg ()
    ; tok (T.TYPEOF, source, yypos, yypos + size yytext));
<INITIAL>"var" =>
   (makeSlashNone ()
    ; tok (T.VAR, source, yypos, yypos + size yytext));
<INITIAL>"void" =>
   (makeSlashReg ()
    ; tok (T.VOID, source, yypos, yypos + size yytext));
<INITIAL>"while" =>
   (makeSlashNone ()
    ; tok (T.WHILE, source, yypos, yypos + size yytext));
<INITIAL>"with" =>
   (makeSlashNone ()
    ; tok (T.WITH, source, yypos, yypos + size yytext));
<INITIAL>"/=" =>
   (if slashIsReg ()
       then (YYBEGIN R
             ; startRegexp (source, yypos)
             ; addRegexpChar #"="
             ; continue ())
    else (makeSlashReg ()
          ; tok (T.ASSIGNOP yytext, source, yypos, yypos + size yytext)));
<INITIAL>"/" =>
   (if slashIsReg ()
       then (YYBEGIN R; startRegexp (source, yypos); continue ())
    else (makeSlashReg ()
          ; tok (T.MULOP yytext, source, yypos, yypos + size yytext)));

<INITIAL>{Identifier} =>
   (makeSlashDiv ()
    ; tok (T.IDENTIFIER yytext, source, yypos, yypos + size yytext));

<INITIAL>"\128"? => (eof arg);
  
<R>"/"{RegularExpressionFlags} =>
   (YYBEGIN INITIAL
    ; makeSlashDiv ()
    ; finishRegexp (yypos + size yytext,
                    {flags = String.dropPrefix (yytext, 1)}));
<R>"\\"{LineTerminator} =>
   (newline (source, yypos + size yytext)
    ; error (source, yypos, yypos + 2, "illegal newline escape in regexp")
    ; continue ());
<R>"\\". => (addRegexpChar #"\\"
             ; addRegexpChar (String.sub (yytext, 1))
             ; continue ());
<R>. => (addRegexpChar (String.sub (yytext, 0)); continue ());

<INITIAL>{Quote} =>
   (startString (source, yypos, String.sub (yytext, 0))
    ; YYBEGIN S
    ; continue ());

<S>{Quote} =>
   (makeSlashDiv ()
    ; (case maybeFinishString (String.sub (yytext, 0), yypos + 1) of
          NONE => continue ()
        | SOME t => (YYBEGIN INITIAL; t)));
<S>"\\'" => (addChar #"'"; continue ());
<S>"\\\"" => (addChar #"\""; continue ());
<S>"\\\\" => (addChar #"\\"; continue ());
<S>"\\b" => (addChar #"\b"; continue ());
<S>"\\f" => (addChar #"\f"; continue ());
<S>"\\n" => (addChar #"\n"; continue ());
<S>"\\r" => (addChar #"\r"; continue ());
<S>"\\t" => (addChar #"\t"; continue ());
<S>"\\v" => (addChar #"\v"; continue ());
<S>"\\0" => (addChar #"\000"; continue ());

<S>"\\"{OctalEscape} => (addOctalChar yytext; continue ());
<S>"\\x"{HexDigit}{2} => (addHexChar yytext; continue ());
<S>"\\u"{HexDigit}{4} => (addHexChar yytext; continue ());
<S>"\\U"{HexDigit}{8} => (addHexChar yytext; continue ());
<S>"\\"{LineTerminator} =>
   (newline (source, yypos + size yytext - 1)
    ; error (source, yypos, yypos + 2, "illegal newline escape in string")
    ; continue ());
<S>"\\". => (addChar (String.sub (yytext, 1)); continue ());
<S>. => (addChar (String.sub (yytext, 0)); continue ());

<INITIAL>. =>
   (error (source, yypos, yypos + 1,
           concat ["illegal token: ", Char.toString (String.sub (yytext, 0))])
    ; continue ());
