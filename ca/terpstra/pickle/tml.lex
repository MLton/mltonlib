val nix = ((), ())
fun eof () = Tokens.EOF nix

type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, unit) token
type arg = unit
type pos = unit

val commentLevel = ref 0

%% 

%reject
%s A;
%header (functor TMLLexFun (structure Tokens : TML_TOKENS));

alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
sym=[-!%&$+/:<=>?@~`^|#*]|"\\";
symId={sym}+;
id={alphanumId}|{symId};
longid={id}("."{id})*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}+;

%%
<INITIAL>{ws}	=> (continue ());
<INITIAL>{eol}	=> (continue ());
<INITIAL>","	=> (Tokens.COMMA nix);
<INITIAL>"{"	=> (Tokens.LBRACE nix);
<INITIAL>"}"	=> (Tokens.RBRACE nix);
<INITIAL>"("	=> (Tokens.LPAREN nix);
<INITIAL>")"	=> (Tokens.RPAREN nix);
<INITIAL>"|"    => (Tokens.BAR nix);
<INITIAL>":"    => (Tokens.COLON nix);
<INITIAL>"="    => (Tokens.EQUALOP nix);
<INITIAL>"and"  => (Tokens.AND nix);
<INITIAL>"datatype" => (Tokens.DATATYPE nix);
<INITIAL>"eqtype" => (Tokens.EQTYPE nix);
<INITIAL>"of" => (Tokens.OF nix);
<INITIAL>"op" => (Tokens.OP nix);
<INITIAL>"type" => (Tokens.TYPE nix);
<INITIAL>"withtype" => (Tokens.WITHTYPE nix);
<INITIAL>"'"{alphanum}? => (Tokens.TYVAR (yytext, (), ()));
<INITIAL>{longid} =>
   (case yytext of
       "*" => Tokens.ASTERISK nix
     | _ => Tokens.LONGID (yytext, (), ()));
<INITIAL>"(*"	=> (YYBEGIN A
                    ; commentLevel := 1
                    ; continue ());
<INITIAL>.	=> (print ("parsing: illegal token\n") ;
		    continue ());

<A>"(*"		=> (commentLevel := !commentLevel + 1; continue ());
<A>"*)"         => (commentLevel := !commentLevel - 1
		    ; if 0 = !commentLevel then YYBEGIN INITIAL else ()
		    ; continue ());
<A>.		=> (continue ());
<A>{ws}		=> (continue ());
<A>{eol}	=> (continue ());
