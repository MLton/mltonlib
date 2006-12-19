fun overlap (NONE, NONE, x, y) = true
  | overlap (SOME l, NONE, x, y) = l < y
  | overlap (NONE, SOME r, x, y) = x < r
  | overlap (SOME l, SOME r, x, y) = x < r andalso l < y

(* assumes overlap *)
fun pick (NONE, NONE, x, y) = x
  | pick (SOME l, NONE, x, y) = y - 1 (* l < y *)
  | pick (NONE, SOME r, x, y) = x (* x < r *)
  | pick (SOME l, SOME r, x, y) = if l < x then x else l

fun edgeLength (l, r) =
  let
    val asciiweights = [
      ( 65,  91,   1), (* uppercase chars are perfect *)
      ( 97,  123,  1), (* lowercase chars are perfect *)
      ( 48,  58,   2), (* digits are nice *)
      ( 32,  33,   3), (* space is better than punctuation *)
      ( 58,  65,   4), (* :;<=>?@ not pretty, but ok *)
      ( 91,  97,   4), (* [\]^_` not pretty, but ok *)
      (123,  127,  4), (* {|}~ not pretty, but ok *)
      ( 33,  48,   4), (* !"#$%&'()*+-,-./ are not pretty, but acceptable *)
      (127,  256, 12), (* anything bigger is not nicely printable *)
      (  1,  32,  25), (* control chars are bad too *)
      (  0,   1, 200)] (* try really hard to avoid nulls *)
    
    val (li, ri) = (Option.map Char.ord l, Option.map Char.ord r)
    fun match (x, y, _) = overlap (li, ri, x, y)
  in
    case valOf (List.find match asciiweights) of (x, y, w) =>
      (w, Char.chr (pick (li, ri, x, y)))
  end

structure A = Automata(Alphabet)
structure RE = A.RegularExpression
structure E = A.Expression
structure DFA = A.Deterministic

fun examine (a, b) =
  let
    val convert = E.toDFA o RE.toExpression o RE.fromString
    val find = Option.map String.implode o DFA.shortestMatch edgeLength
    val join = find o DFA.optimize o DFA.intersect
    val (pa, pb) = (convert a, convert b)
    val (na, nb) = (DFA.complement pa, DFA.complement pb)
    val (pas, nas, pbs, nbs) = (find pa, find na, find pb, find nb)
    val (papbs, panbs, napbs, nanbs) = 
          (join (pa, pb), join (pa, nb), join (na, pb), join (na, nb))
    
    fun length (SOME x) = 4 + String.size x
      | length NONE = 3
    fun max (x, y) = if x < y then y else x
    fun biggest (x, y, z) =  max (length x, max (length y, length z))
    val col1 = biggest(NONE, pas, nas)
    val col2 = biggest(pbs, papbs, napbs)
    val col3 = max(biggest(nbs, panbs, nanbs), 8)
    
    fun whitespace 0 = ""
      | whitespace i = " " ^ whitespace (i-1)
    fun dashes 0 = ""
      | dashes i = "-" ^ dashes (i-1)
    fun format (s, w) = 
      let val pad = w - String.size s in
      whitespace (pad div 2) ^ s ^ whitespace ((pad+1) div 2) end
    fun entry (SOME x, w) = format ("\"" ^ x ^ "\"", w)
      |	entry (NONE, w) = format ("-", w)
    
    val setrelation = case (papbs, panbs, napbs, nanbs) of
        (_, NONE, NONE, _) => "A is identical to B"
      | (NONE, _, _, NONE) => "A is the complement of B"
      | (_, NONE, _, _) => "A is a subset of B"
      | (_, _, NONE, _) => "A is a superset of B"
      | (NONE, _, _, _) => "A is disjoint from B"
      | _ => "A overlaps B"
  in
    print ("Expression A (" ^ Int.toString (DFA.size pa) ^ " states) = \"" ^ a ^ "\"\n");
    print ("Expression B (" ^ Int.toString (DFA.size pb) ^ " states) = \"" ^ b ^ "\"\n");
    print "\n";
    print ("       |" ^ whitespace col1  ^ "|" ^ format("B", col2)  ^ "|" ^ format("not(B)", col3) ^ "\n");
    print ("--------" ^ dashes col1      ^ "-" ^ dashes col2        ^ "-" ^ dashes col3        ^ "\n");
    print ("       |" ^ whitespace col1  ^ "|" ^ entry(pbs, col2)   ^ "|" ^ entry(nbs, col3)   ^ "\n");
    print ("A      |" ^ entry(pas, col1) ^ "|" ^ entry(papbs, col2) ^ "|" ^ entry(panbs, col3) ^ "\n");
    print ("not(A) |" ^ entry(nas, col1) ^ "|" ^ entry(napbs, col2) ^ "|" ^ entry(nanbs, col3) ^ "\n");
    print "\n";
    print ("Set relationship: " ^ setrelation ^ ".\n")
  end

val ()  = case CommandLine.arguments () of
    (a :: b :: []) => examine (a, b)
  | _ => print "Expect two regular expressions for arguments\n"
