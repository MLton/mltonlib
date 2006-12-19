signature ALPHABET =
  sig
    eqtype char
    eqtype string
    
    val ord: char -> int
    val chr: int -> char
    
    val < : (char * char) -> bool
    val foldl: (char * 'a -> 'a) -> 'a -> string -> 'a
  end

structure Alphabet =
  struct
    type char = char
    type string = string
    
    val ord = Char.ord
    val chr = Char.chr
    
    val (op <) = Char.<
    fun foldl f a s = Substring.foldl f a (Substring.full s)
  end  

functor Automata(Alphabet : ALPHABET) : AUTOMATA 
  where type char   = Alphabet.char
  and   type ZTree.key = Alphabet.char
  and   type string = Alphabet.string =
  struct
    structure AlphaOrder = 
      struct 
        type t = Alphabet.char 
        val (op <) = Alphabet.<
      end
    structure StateOrder =
      struct
        type t = int
        val (op <) = Int.<
      end
    structure ZTree = ZTree(AlphaOrder)
    structure BTree = BTree(StateOrder)
    open Alphabet
    
    fun printSML (f, i, ZTree.Leaf v, tail) = f v :: tail
      | printSML (f, i, ZTree.Node (l, k, r), tail) =
          "\n" :: i :: "if c < chr " :: Int.toString (ord k) :: " then " ::
          printSML (f, i ^ "  ", l, 
            "\n" :: i :: "else " ::
            printSML (f, i ^ "  ", r, tail))
    
    fun toString c = "state" ^ Int.toString c
    fun printC (i, ZTree.Leaf v, tail) = "goto " :: toString v :: ";" :: tail
      | printC (i, ZTree.Node (l, k, r), tail) =
          "\n" :: i :: 
          "if (*s < " :: (Int.toString o ord) k :: ") " ::
          printC (i ^ "\t", l, 
            "\n" :: i :: "else " ::
            printC (i ^ "\t", r, tail))
    
    fun dotNode (i, (b, _), tail) = 
          "\t" :: Int.toString i :: " [label=\"\"" ::
          (if i = 0 then ",shape=diamond" else "") ::
          (if b then ",fillcolor=green" else "") :: 
          "]\n" :: tail
        
    structure Deterministic =
      struct
        type state = int
        type t = (bool * state ZTree.t) vector
        
        fun size a = Vector.length a
        fun start _ = 0
        fun accepts a x = case Vector.sub (a, x) of (b, _) => b
        fun step a (c, x) = ZTree.lookup (#2 (Vector.sub (a, x))) c
        fun multistep a (s, x) = foldl (step a) x s
        fun test a s = accepts a (multistep a (s, start a))
        
        val empty = Vector.fromList [ 
          (true, ZTree.uniform 1),
          (false, ZTree.uniform 1) ]
        val any = Vector.fromList [ 
          (false,  ZTree.uniform 1),
          (true, ZTree.uniform 2),
          (false, ZTree.uniform 2) ]
        fun char t = Vector.fromList [
          (false, ZTree.map (fn true => 1 | false => 2) t),
          (true, ZTree.uniform 2),
          (false, ZTree.uniform 2) ]
        
        fun mapPair f (x, y) = (f x, f y)
        
        (* f maps old state to new, n is the number of cells to keep *)
        fun mapStates (f, n) a =
          let
            open ZTree
            val v = Array.tabulate (Vector.length a, fn _ => (true, uniform 0))
            val fixtree = fromFront o uniq (op =) o imap f o front
            fun map (i, (b, t)) = Array.update (v, f i, (b, fixtree t))
            val () = Vector.appi map a
          in
            Vector.tabulate (n, fn i => Array.sub (v, i))
          end
        
        (* eliminate unreachable states -- and put states in canonical order *)
        fun unreachable a =
          let
            val l = Vector.length a
            val v = Array.tabulate (l, fn _ => false)
            val m = Array.tabulate (l, fn _ => l - 1)
            val e = ref 0
            fun dfs i = 
              if Array.sub (v, i) then () else (
                Array.update (v, i, true);
                Array.update (m, i, !e);
                e := (!e + 1);
                ZTree.app dfs (#2 (Vector.sub (a, i)))
              )
            val () = dfs 0
          in
            mapStates (fn i => Array.sub (m, i), !e) a
          end
        
        (* detect and merge duplicate states *)
        fun finddups a = 
          let
            val len = size a
            fun toPair i = (i mod len, i div len)
            fun ofPair (r, c) = len * c + r
            fun agree (r, c) = accepts a r = accepts a c
            val v = Array.tabulate (len*len, agree o toPair)
            
            open ZTree
            fun tree i = #2 (Vector.sub (a, i))
            fun fold a (Iter (b, NONE, _)) = b andalso a
              | fold a (Iter (b, SOME _, iter)) = fold (b andalso a) (iter ())
            fun match (r, c) = Array.sub (v, ofPair (r, c))
            fun distinct (i, b) = b andalso 
              (fold true o merge match o mapPair (front o tree) o toPair) i
            
            val changed = ref true
            fun update (i, b) = let val n = distinct (i, b) in
              (changed := (!changed orelse (n <> b)); n) end
            fun pass () = Array.modifyi update v
            val () = while (!changed) do (changed := false; pass ())
            
            (* m stores new state name *)
            val m = Array.tabulate (len, fn _ => 0)
            val e = ref 0
            fun whoAmI (i, j) = 
              if Array.sub (v, ofPair (i, j)) then j else whoAmI (i, j+1)
            fun setState (i, _) = 
              let val j = whoAmI (i, 0) in
                if i = j then (!e before e := (!e + 1)) 
                else Array.sub (m, j)
              end
            val () = Array.modifyi setState m
          in
            mapStates (fn i => Array.sub (m, i), !e) a
          end
        
        (* the second unreachable step puts the DFA in canonical order *)
        val optimize = unreachable o finddups o unreachable
        
        (* more interesting would be to output an example difference *)
        fun equal (v1, v2) = Vector.foldli 
          (fn (i, (b1, t1), a) => 
            case Vector.sub (v2, i) of (b2, t2) =>
              a andalso b1 = b2 andalso ZTree.equal (op =) (t1, t2)) 
          true v1
        
        fun crossproduct (a, b, f) =
          let
            open ZTree
            val (rows, cols) = (Vector.length a, Vector.length b)
            fun toPair i = (i mod rows, i div rows)
            fun ofPair (r, c) = rows * c + r
            fun getState (r, c) = (Vector.sub (a, r), Vector.sub (b, c))
            val tree = fromFront o uniq (op =) o merge ofPair o mapPair front
            fun cross ((b1, t1), (b2, t2)) = (f (b1, b2), tree (t1, t2))
          in
            Vector.tabulate (rows*cols, cross o getState o toPair)
          end
        
        fun complement a = Vector.map (fn (b, t) => (not b, t)) a
        fun union (a, b) = crossproduct (a, b, fn (a, b) => a orelse b)
        fun intersect (a, b) = crossproduct (a, b, fn (a, b) => a andalso b)
        
        (* Find the lowest weight string which matches the expression *)
        fun shortestMatch edgeweight a = 
          let
            val n = Vector.length a
            val parent  = Array.tabulate (n, fn _ => (0, chr 0))
            val weight  = Array.tabulate (n, fn _ => 1999999999)
            val visited = Array.tabulate (n, fn _ => false)
            val () = Array.update(weight, 0, 0) (* start at empty string *)
            
            val nextNode = Array.foldli 
              (fn (i, w, (bi, bw)) => 
                if not (Array.sub (visited, i)) andalso Int.< (w, bw)
                then (i, w) else (bi, bw)) 
              (~1, 1999999999)
            
            fun relaxEdges (i, vw) = ZTree.fold
              (fn (l, j, r, ()) => case edgeweight (l, r) of (ew, c) =>
                if vw + ew >= Array.sub (weight, j) then () else (
                  Array.update (weight, j, vw + ew);
                  Array.update (parent, j, (i, c))))
              ()
              (case Vector.sub (a, i) of (_, t) => t)
            
            val working = ref true
            val () = while (!working) do
              let
                val (i, w) = nextNode weight
              in
                if i = ~1 then working := false else (
                  Array.update (visited, i, true);
                  relaxEdges (i, w))
              end
            
            val shortestAccept = Array.foldli
              (fn (i, w, (bi, bw)) =>
                if #1 (Vector.sub (a, i)) andalso Int.< (w, bw)
                then (i, w) else (bi, bw))
              (~1, 1999999999) weight
            
            fun followTrail (0, tail) = tail
              | followTrail (i, tail) = 
                  case Array.sub (parent, i) of (p, c) => 
                    followTrail (p, c :: tail)
          in
            if  #1 shortestAccept = ~1 then NONE else
            SOME (followTrail (#1 shortestAccept, []))
          end
        
        fun dotEdge (i, (_, t), tail) = 
          let
            val toString = String.toCString o Char.toString o Char.chr o ord
            fun pred NONE = NONE | pred (SOME x) = SOME (chr (ord x - 1))
            fun fmt NONE = "" | fmt (SOME x) = toString x
            fun fmtp (SOME x, SOME y) =
                  if x = y then toString x else toString x ^ "-" ^ toString y
              |	fmtp (x, y) = fmt x ^ "-" ^ fmt y
            fun append (l, v, r, tree) = 
              case BTree.get tree v of
                  NONE => BTree.insert tree (v, [fmtp (l, pred r)])
                | SOME x => BTree.insert tree (v, fmtp (l, pred r) :: x)
            val edges = BTree.map (String.concatWith ",")
              (ZTree.foldr append BTree.empty t)
            fun print (j, l, tail) = 
              "\t" ::Int.toString i :: "->" :: Int.toString j  ::
              " [label=\"" :: l :: "\"]\n" :: tail
          in
            BTree.foldr print tail edges
          end
          
        fun toDot (n, a) = String.concat (
          "strict digraph " :: n :: " {\n" ::
          "\tnode [style=filled,fillcolor=grey,shape=circle]\n" ::
          Vector.foldri dotNode 
            (Vector.foldri dotEdge ["}\n"] a) a)
        
        fun toSML (n, a) = String.concat (
          "fun step s =\n" ::
          "  let\n" ::
          "    datatype x = F of (char -> x)\n" ::
          "    fun eval s = foldl (fn (c, F f) => f c) (F step0) s" ::
          Vector.foldri 
            (fn (i, (b, t), tail) => 
              "\n    and step" :: Int.toString i :: " c = " ::
              printSML (fn i => ("F step" ^ Int.toString i), "      ", t, tail))
            ("\n" ::
             "  in\n" ::
             "    case eval s of F f => f\n" ::
             "  end\n" :: 
             nil) 
            a)
        
        fun bodyC (i, (b, t), tail) =
          "\n" :: toString i :: ":\n" ::
          "\tif (++s == e) return " ::
          (if b then "1" else "0") :: ";\n\t" ::
           printC ("\t", t, tail)
        fun caseC (i, (b, ZTree.Leaf v), tail) =
              if i = v then 
                "\n" :: toString i :: ": return " ::
                (if b then "1" else "0") :: ";\n" :: tail
              else bodyC (i, (b, ZTree.Leaf v), tail)
          | caseC (i, (b, t), tail) = bodyC (i, (b, t), tail)
        fun toC (n, a) = String.concat (
          "int " :: n :: "(const unsigned char* s, const unsigned char* e) {\n" ::
          "\t--s;" ::
          Vector.foldri caseC
            ["\n}\n"]
            a)
      end
      
    structure NonDeterministic =
      struct
        type state = Deterministic.state
        type t = state list vector * Deterministic.t
        
        (* note: the output is sorted b/c it was in a btree *)
        fun dfs e q =
          let
            open BTree
            fun touch (t, []) = t
              | touch (t, a :: r) = 
                if isSome (get t a) then touch (t, r) else
                touch (insert t (a, ()), Vector.sub (e, a) @ r)
          in
            fold (fn (k, _, l) => k :: l) [] (touch (empty, q))
          end
        
        fun size (_, a) = Vector.length a
        fun start _ = 0
        fun accepts (_, a) x = Deterministic.accepts a x
        fun step (e, a) (c, l) = dfs e
          (List.map (fn x => Deterministic.step a (c, x)) l)
        fun multistep a (s, x) = foldl (step a) x s
        fun test a s = List.exists (accepts a) (multistep a (s, [start a]))
        
        (* set all accept states to have epsilon transitions to s *)
        fun mapAccept s (e, a) = 
          let
            fun mapEpsilon (i, l) = if accepts (e, a) i then s :: l else l
            fun noAccept a = Vector.map (fn (_, x) => (false, x)) a
          in
            (Vector.mapi mapEpsilon e, noAccept a)
          end
        
        fun mapRenumber x (e, a) =
          let
            val e = Vector.map (List.map (fn i => i + x)) e
            fun stateRelabel (b, t) = (b, ZTree.map (fn i => i + x) t)
          in
            (e, Vector.map stateRelabel a)
          end
        
        (* Scheme: new start state s accepts and -> all old starts, accepts -> s*)
        fun power (e, a) = 
          let
            val (e, a) = (mapAccept 0 o mapRenumber 2) (e, a)
            val e0 = Vector.fromList [[2], []]
            val a0 = Vector.fromList [(true,  ZTree.uniform 1), 
                                      (false, ZTree.uniform 1)]
          in
            (Vector.concat [e0, e], Vector.concat [a0, a])
          end
        
        (* Scheme: s1 = start states, v1 accept states -> s2 start states *)
        fun concat ((e1, a1), (e2, a2)) =
          let
            val l1 = Vector.length a1
            val (e1, a1) = mapAccept   l1 (e1, a1)
            val (e2, a2) = mapRenumber l1 (e2, a2)
          in
            (Vector.concat [e1, e2], Vector.concat [a1, a2])
          end
        
        fun fromDFA a = (Vector.tabulate (Vector.length a, fn _ => []), a)
        
        (* The general NFA->DFA conversion algorithm works as follows:
         *   - we start by calling getName (dfs e [0])
         *   - getName checks for an existing integer mapping for the list
         *     if one exists, the integer is returned
         *     otherwise:
         *       - the next available integer is allocated to this list
         *       - we merge all trees for the named states in the list
         *         via a hierachical combination of ZTree.merge
         *       - the new int list ZTree.iterator is imap'd with dfs
         *       - then we uniq the operation, and imap mapName it
         *         (this recursively explores other reachable subset states)
         *       - the new iterator is fromFront'd to create the tree.
         *       - if any of the states in the list accept, this accepts too
         *)
        structure Names = 
          struct
            type t = int vector
            fun < (l, r) = Vector.collate Int.compare (l, r) = LESS
          end
        structure NTree = BTree(Names)
        fun toDFA (e, a) =
          let
            val names = ref NTree.empty
            val number = ref 0
            
            fun buildTree v =
              let
                open ZTree
                datatype tree = Leaf of int | Node of tree * tree
                fun flatten tail (Leaf i) = i :: tail
                  | flatten tail (Node (l, r)) = flatten (flatten tail r) l
                
                fun getIter i = front (#2 (Vector.sub (a, Vector.sub (v, i))))
                
                fun grow (l, r) =
                  if l + 1 = r then imap Leaf (getIter l) else
                  let val m = (l+r) div 2 in
                    merge Node (grow (l, m), grow (m, r))
                  end
              in
                (fromFront o uniq (op =) o imap (mapName o dfs e o flatten []) o grow)
                (0, Vector.length v)
              end
            and mapName l =
              let
                val v = Vector.fromList l
              in
                case NTree.get (!names) v of
                    SOME (i, _, _) => i
                  | NONE =>
                      let 
                        val me = !number before (number := !number + 1)
                        val () = names := NTree.insert (!names)
                          (v, (me, false, ZTree.uniform 0)) (* store name *)
                        val value = 
                          (me, List.exists (accepts (e, a)) l, buildTree v)
                        val () = names := NTree.insert (!names) (v, value)
                      in
                        me
                      end
              end
            
            val _ = mapName (dfs e [0])
            val d = Array.tabulate (!number, fn _ => (false, ZTree.uniform 0))
            val () = NTree.app 
              (fn (i, b, t) => Array.update (d, i, (b, t))) (!names)
            
(*
            fun fmt NONE = ()
              | fmt (SOME c) = (print o Char.toString o Char.chr o ord) c
            fun treedump (l, v, r, ()) = (
              fmt l; print "-"; fmt r; print ":"; 
              print (Int.toString v ^ " "))
            fun debug (v, (i, b, t)) = (
              print "States ";
              Vector.map (print o Int.toString) v;
              print (": (" ^ Int.toString i ^ ", " ^ Bool.toString b ^ ", ");
              ZTree.fold treedump () t;
              print ")\n")
            val () = NTree.appk debug (!names)
*)
          in
            Array.vector d
          end
        
        fun dotEpsilon (i, [], tail) = tail
          | dotEpsilon (i, h :: r, tail) = 
              "\t" :: Int.toString i :: "->" :: Int.toString h :: "\n" :: tail
        fun toDot (n, (e, a)) = String.concat (
          "digraph " :: n :: " {\n" ::
          "\tnode [style=filled,fillcolor=grey,shape=circle]\n" ::
          Vector.foldri dotNode
            (Vector.foldri Deterministic.dotEdge 
              ("\tedge [style=dashed]\n" ::
               Vector.foldri dotEpsilon ["}\n"] e) a) a)
      end
    
    structure Expression =
      struct
        datatype t = 
          Empty | Any | Char of bool ZTree.t | Not of t | Star of t |
          Concat of t * t | Union of t * t | Intersect of t * t
        
        structure DFA = Deterministic
        structure NFA = NonDeterministic
        
        fun toDFA Empty = DFA.empty
          | toDFA Any = DFA.any
          | toDFA (Char t) = DFA.char t
          | toDFA (Not e) = DFA.complement (toDFA e)
          | toDFA (Star e) = 
              (DFA.optimize o NFA.toDFA o NFA.power o NFA.fromDFA o toDFA) e
          | toDFA (Concat (e1, e2)) =
              (DFA.optimize o NFA.toDFA o NFA.concat)
              (NFA.fromDFA (toDFA e1), NFA.fromDFA (toDFA e2))
          | toDFA (Union (e1, e2)) =
              (DFA.optimize o DFA.union) (toDFA e1, toDFA e2)
          | toDFA (Intersect (e1, e2)) =
              (DFA.optimize o DFA.intersect) (toDFA e1, toDFA e2)
        
(*
        fun toString Empty = ""
          | toString Any = "."
          | toString (Char c) = Char.toString (Char.chr (ord c))
          | toString (Not e) = "^(" ^ toString e ^ ")"
          | toString (Star e) = "(" ^ toString e ^ ")*"
          | toString (Concat (e1, e2)) = toString e1 ^ toString e2
          | toString (Union (e1, e2)) = "(" ^ toString e1 ^ ")+(" ^ toString e2 ^ ")"
          | toString (Intersect (e1, e2)) = "(" ^ toString e1 ^ ")-(" ^ toString e2 ^ ")"
*)
      end
    
    structure RegularExpression =
      struct
        structure E = Expression
        type char = Char.char
        (* BNF:
           exp = branch 
                 branch '|' exp
           branch = empty
                    piece
                    piece branch
           piece = atom ('*' | '+' | '?' | bound)?
           bound = '{' int (',' int?)? '}'
           atom = '(' exp ')'
                  bracket
                  '^'
                  '$'
                  '\' char
                  char
                  '{' (* if not followed by integer... *)
           bracket = '[' '^'? (']')? (col | equiv | class | range | char)* ']'
           col = '[.' chars '.]'
           equiv = '[=' chars '=]'
           class = '[:' chars ':]'
           range = char '-' char
         *)
        
        datatype bracket =
          Elt of char | End | Not of bracket | Range of char * char |
          Alt of bracket * bracket
          
        datatype t = 
          Union of t * t | Star of t | Plus of t | Option of t | Paran of t |
          Concat of t * t | Char of char | Any | Empty | 
          Bound of t * int * int option | Bracket of bracket
        
        fun cvtBound (e, 0, NONE) = E.Star e
          | cvtBound (e, i, NONE) = E.Concat (e, cvtBound (e, i-1, NONE))
          | cvtBound (e, 0, SOME 0) = E.Empty
          | cvtBound (e, 0, SOME j) = E.Union (E.Empty, cvtBound (e, 1, SOME j))
          | cvtBound (e, i, SOME j) =  E.Concat (e, cvtBound (e, i-1, SOME (j-1)))
          
        fun cvtBracket (Elt c) = cvtBracket (Range (c, c))
          | cvtBracket (Not b) = ZTree.map not (cvtBracket b)
          | cvtBracket End = ZTree.uniform false
          | cvtBracket (Range (l, h)) = 
              ZTree.range (false, chr (Char.ord l), chr (Char.ord h + 1), true)
          | cvtBracket (Alt (b1, b2)) = 
              (ZTree.fromFront o ZTree.uniq (op =) o 
               ZTree.merge (fn (x,y) => x orelse y))
              (ZTree.front (cvtBracket b1), ZTree.front (cvtBracket b2))
        
        fun exp (Union (e1, e2)) = E.Union (exp e1, exp e2)
          | exp (Concat (e1, e2)) = E.Concat (exp e1, exp e2)
          | exp (Star e) = E.Star (exp e)
          | exp (Plus e) = let val e = exp e in E.Concat (e, E.Star e) end
          | exp (Option e) = E.Union (E.Empty, exp e)
          | exp (Paran e) = exp e
          | exp (Char c) = E.Char (cvtBracket (Elt c))
          | exp (Bound (e, l, r)) = cvtBound (exp e, l, r)
          | exp (Bracket b) = E.Char (cvtBracket b)
          | exp Any = E.Any
          | exp Empty = E.Empty
        val toExpression = exp
        
        fun fromString s = 
          case parse_exp (String.explode s) of 
              (e, []) => e
            | (e, l) => (
              print ("Failed to parse: " ^ String.implode l ^ "\n"); 
              e)
        and parse_exp ts =
          case parse_branch ts of
              (branch, #"|" :: ts') => 
                let val (exp, ts'') = parse_exp ts' 
                in (Union (branch, exp), ts'') end
            | (branch, ts'') => (branch, ts'')
        and parse_branch ts =
          case parse_piece ts of
              (SOME p, ts') => 
                let val (r, ts'') = parse_branch ts' 
                in (Concat (p, r), ts'') end
            | (NONE, _) => (Empty, ts)
        and parse_piece ts =
          case parse_atom ts of
              (SOME a, #"*" :: ts') => (SOME (Star a), ts')
            | (SOME a, #"+" :: ts') => (SOME (Plus a), ts')
            | (SOME a, #"?" :: ts') => (SOME (Option a), ts')
            | (SOME a, #"{" :: ts') => 
                (case parse_bound a ts' of
                     (SOME b, ts'') => (SOME b, ts'')
                   | (NONE, _) => (SOME a, #"{" :: ts'))
            | (SOME a, ts') => (SOME a, ts')
            | (NONE, _) => (NONE, ts)
        and parse_bound a ts =
          case parse_int ts of
              (SOME i, _, #"," :: #"}" :: ts') => 
                (SOME (Bound (a, i, NONE)), ts')
            | (SOME i, _, #"," :: ts') =>
                (case parse_int ts' of
                    (SOME j, _, #"}"::ts'') => 
                      if i <= j then
                           (SOME (Bound (a, i, SOME j)), ts'')
                      else (NONE, ts)
                  | (SOME j, _, _) => (NONE, ts)
                  | (NONE, _, _) => (NONE, ts))
            | (SOME i, _, #"}" :: ts') => (SOME (Bound (a, i, SOME i)), ts')
            | (SOME i, _, _) => (NONE, ts)
            | (NONE, _, _) => (NONE, ts)
        and parse_int ts =
          case parse_digit ts of
              (SOME i, ts') =>
                (case parse_int ts' of
                    (SOME j, p, ts'') => (SOME (i*p+j), p*10, ts'')
                  | (NONE, _, _) => (SOME i, 10, ts'))
            | (NONE, _) => (NONE, 1, ts)
        and parse_digit ts =
          case ts of
              (#"0" :: ts') => (SOME 0, ts')
            | (#"1" :: ts') => (SOME 1, ts')
            | (#"2" :: ts') => (SOME 2, ts')
            | (#"3" :: ts') => (SOME 3, ts')
            | (#"4" :: ts') => (SOME 4, ts')
            | (#"5" :: ts') => (SOME 5, ts')
            | (#"6" :: ts') => (SOME 6, ts')
            | (#"7" :: ts') => (SOME 7, ts')
            | (#"8" :: ts') => (SOME 8, ts')
            | (#"9" :: ts') => (SOME 9, ts')
            | _ => (NONE, ts)
        and parse_atom ts =
          case ts of
              (#"(" :: ts') => 
                (case parse_exp ts' of
                     (exp, #")" :: ts'') => (SOME (Paran exp), ts'')
                   | (exp, _) => (NONE, #"(" :: ts')) (* warn!!! *)
            | (#"\\" :: x :: ts'') => (SOME (Char x), ts'')
            | (#"." :: ts'') => (SOME Any, ts'')
            | (#"[" :: ts') =>
                (case parse_bnot ts' of
                    (bracket, #"]" :: ts'') => (SOME (Bracket bracket), ts'')
                  | (_, _) => (NONE, #"[" :: ts')) (* warn!!! *)
            | (#")" :: ts') => (NONE, #")" :: ts')
            | (#"|" :: ts') => (NONE, #"|" :: ts')
            | (x :: ts') => (SOME (Char x), ts')
            | [] => (NONE, ts)
        and parse_bnot ts = 
          case ts of
              (#"^" :: ts') => 
                let val (r, ts'') = parse_bclose ts'
                in (Not r, ts'') end
            | _ => parse_bclose ts
        and parse_bclose ts =
          case ts of
              (#"]" :: ts') => 
                let val (r, ts'') = parse_blist ts'
                in (Alt (Elt #"]", r), ts'') end
            | _ => parse_blist ts
        and parse_blist ts =
          case parse_batom ts of
              (SOME a, ts') =>
                let val (r, ts'') = parse_blist ts'
                in (Alt (a, r), ts'') end
            | (NONE, _) => (End, ts)
        and parse_batom ts =
          case ts of
              (c :: #"-" :: #"]" :: ts') => (SOME (Elt c), tl ts)
            | (#"]" :: ts') => (NONE, ts)
            | (c :: #"-" :: d :: ts') => (SOME (Range (c, d)), ts')
            | (c :: ts') => (SOME (Elt c), ts')
            | _ => (NONE, ts) (* warn!!! *)
      end
  end
