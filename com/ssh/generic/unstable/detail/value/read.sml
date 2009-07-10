(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithRead (Arg : WITH_READ_DOM) : READ_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infixr 6 <^> <+>
   infixr 5 <$> <$$> </> <//>
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse >> >>= >>& >>*
   infix  0 & <|>
   infixr 0 -->
   (* SML/NJ workaround --> *)

   infix 1 ->> >>- <<< >>>

   structure Parsec = MkParsec
     (structure Sequence = struct
         structure Pos = Univ
         structure Elem = Char
         type t = (Elem.t, Pos.t) Reader.t * Pos.t
         val pos = Pair.snd
         fun get (r, s) =
             case r s
              of NONE        => NONE
               | SOME (c, s) => SOME (c, (r, s))
      end
      structure State = Unit)
   open Parsec

   val E = sat o eq

   fun L l = fromReader let
      fun lp i s =
          if i >= size l
          then SOME ((), s)
          else case Sequence.get s
                of NONE         => NONE
                 | SOME (c, s') =>
                   if c = String.sub (l, i)
                   then lp (i+1) s'
                   else NONE
   in
      lp 0
   end

   val skipSpaces = skipManySatisfy Char.isSpace

   fun ignored 0 = skipSpaces >> (L"(*" >> eta ignored 1 <|> return ())
     | ignored n = L"*)" >> eta ignored (n-1) <|>
                   L"(*" >> eta ignored (n+1) <|>
                   elem  >> eta ignored n

   val ignored = ignored 0

   fun l >>> r = l >> ignored >> r
   fun l <<< r = l >>= (fn l => ignored >> r >> return l)

   fun parens p = guess (E#"(" >>> eta parens p) <<< E#")" <|> p

   datatype radix = datatype StringCvt.radix

   fun id first rest = map implode (many1Satisfy2 first rest)

   val alphaId = id Char.isAlpha
                    (fn c => Char.isAlphaNum c
                             orelse #"'" = c orelse #"_" = c)
   val isSymbolic = Char.contains "!#$%&*+-/:<=>?@\\^`|~"
   val symbolicId = map implode (many1Satisfy isSymbolic)

   val shortId = alphaId <|> symbolicId
   val longId = sepBy1 shortId (E#".")
   fun I s = shortId >>= (fn i => if i = s then return () else zero)

   val numLabel = id (Char.inRange (#"1", #"9")) Char.isDigit
   val label = shortId <|> numLabel

   fun mkSequ pre suf (Ops.S {fromList, ...}) p = let
      fun fin xs _ = return (fromList (rev xs))
      fun aft xs = ignored >> (E#"," >>> bef xs <|> suf >>= fin xs)
      and bef xs = p >>= (fn x => aft (x::xs))
   in
      parens (pre >>> (suf >>= fin [] <|> bef []))
   end

   fun mkReal (Ops.R {scan, ...} : ('r, 'w, Sequence.t) Ops.r) : 'r t =
       parens (fromScan scan)

   fun mkScalar scan mk = parens (mk (fromScan o scan))

   fun mkWord (Ops.W {scan, ...} : ('w, Sequence.t) Ops.w) : 'w t =
       mkScalar scan (fn p => L"0w" >> (E#"x" >> p HEX <|>
                                        E#"o" >> p OCT <|>
                                        E#"b" >> p BIN <|>
                                                 p DEC))

   fun mkInt (Ops.I {scan, ...} : ('i, Sequence.t) Ops.i) : 'i t =
       mkScalar scan (fn p => peek (L"~0x" <|> L"0x") >> p HEX <|>
                              peek (L"~0o" <|> L"0o") >> p OCT <|>
                              peek (L"~0b" <|> L"0b") >> p BIN <|>
                                                         p DEC)

   fun C c p s = if s = Generics.Con.toString c then SOME p else NONE

   structure ReadRep = LayerRep
     (open Arg
      type  'a      t = 'a t
      type  'a      s = String.t -> 'a t Option.t
      type ('a, 'k) p =
           Int.t -> {fromLabel : 'k -> (Int.t * Univ.t t) Option.t,
                     fromArray : Univ.t Option.t Array.t -> 'a})

   open ReadRep.This

   fun reader' pA rC s =
       case Univ.Iso.new ()
        of (to, from) =>
           Sum.map (from, fn (v, ((_, s), _)) => (v, from s))
                   (parse (ignored >> pA)
                          ((Reader.mapState (from, to) rC, to s),
                           ()))

   fun reader t = reader' (getT t)

   local
      fun error s = let
         val pos = StringSequence.pos s
         val str = StringSequence.vector s
         val size = String.size str
         val begin = Int.max (0, pos - 5)
         val beyond = Int.min (pos + 5, size)
         fun substr b e = String.toString (String.substring (str, b, e-b))
         fun dotsUnless b = if b then "" else "..."
      in
         fails ["parse error at ", Int.toString pos, " (\"",
                dotsUnless (0 = begin),
                substr begin pos, ".", substr pos beyond,
                dotsUnless (size = beyond),
                "\")"]
      end
   in
      fun read t =
          (fn INR (x, s) =>
              if StringSequence.pos s = size (StringSequence.vector s)
              then x
              else error s
            | INL s => error s) o
          reader' (getT t >>- ignored)
                  StringSequence.get o
          StringSequence.full
   end

   structure Open = LayerDepCases
     (fun iso bT (_, b2a) = map b2a (getT bT)
      fun isoProduct bP (_, b2a) =
          (fn {fromLabel, fromArray} =>
              {fromLabel = fromLabel,
               fromArray = b2a o fromArray}) o getP bP
      fun isoSum bS (_, b2a) = Option.map (map b2a) o getS bS

      fun op *` (aP, bP) = let
         val aN = Arg.numElems aP
         val aP = getP aP
         val bP = getP bP
      in
         fn i => let
               val {fromLabel = aL, fromArray = aA} = aP i
               val {fromLabel = bL, fromArray = bA} = bP (i+aN)
            in
               {fromLabel = fn l => case aL l of NONE => bL l | other => other,
                fromArray = fn a => aA a & bA a}
            end
      end
      fun T t = let
         val (to, from) = Univ.Iso.new ()
         val p = map to (getT t)
      in
         fn i =>
            {fromLabel = fn l => if l = i then SOME (i, p) else NONE,
             fromArray = fn a => from (valOf (Array.sub (a, i)))}
      end
      fun R l t = let
         val (to, from) = Univ.Iso.new ()
         val p = map to (getT t)
         val l = Generics.Label.toString l
      in
         fn i =>
            {fromLabel = fn l' => if l' = l then SOME (i, p) else NONE,
             fromArray = fn a => from (valOf (Array.sub (a, i)))}
      end
      fun tuple aP = let
         val {fromLabel, fromArray} = getP aP 0
         val n = Arg.numElems aP
         fun pl a i =
             if i >= n
             then E#")" >> return (fromArray a)
             else case fromLabel i
                   of NONE        => fail "impossible"
                    | SOME (j, p) =>
                      p >>= (fn x =>
                      (Array.update (a, j, SOME x)
                     ; (if i+1 = n
                        then ignored
                        else ignored >> E#"," >> ignored) >> pl a (i+1)))
         fun rl a i =
             if i >= n
             then E#"}" >> return (fromArray a)
             else numLabel >>= (fn l =>
                  case fromLabel (valOf (Int.fromString l) - 1)
                   of NONE        => zero
                    | SOME (j, p) =>
                      if isSome (Array.sub (a, j))
                      then zero
                      else ignored >> I"=" >>> p >>= (fn x =>
                           (Array.update (a, j, SOME x)
                          ; (if i+1 = n
                             then ignored
                             else ignored >> E#"," >> ignored) >> rl a (i+1))))
      in
         parens (E#"(" >>> (fn ? => pl (Array.array (n, NONE)) 0 ?) <|>
                 E#"{" >>> (fn ? => rl (Array.array (n, NONE)) 0 ?))
      end
      fun record aP = let
         val {fromLabel, fromArray} = getP aP 0
         val n = Arg.numElems aP
         fun lp a i =
             if i >= n
             then E#"}" >> return (fromArray a)
             else label >>= (fn l =>
                  case fromLabel l
                   of NONE        => zero
                    | SOME (j, p) =>
                      if isSome (Array.sub (a, j))
                      then zero
                      else ignored >> I"=" >>> p >>= (fn x =>
                           (Array.update (a, j, SOME x)
                          ; (if i+1 = n
                             then ignored
                             else ignored >> E#"," >> ignored) >> lp a (i+1))))
      in
         parens (E#"{" >>> (fn ? => lp (Array.array (n, NONE)) 0 ?))
      end

      fun op +` (lS, rS) = let
         val l = getS lS
         val r = getS rS
      in
         fn s =>
            case l s
             of SOME l => SOME (map INL l)
              | NONE   => Option.map (map INR) (r s)
      end
      val unit = E#"(" >>> parens (E#")" >> return ())
      fun C0 c = C c (return ())
      fun C1 c t = C c (ignored >> getT t)
      fun data tS =
          case getS tS
           of t => parens (parens longId >>= (fn s =>
                           case t (String.concatWith "." s)
                            of NONE   => zero
                             | SOME p => p))

      val Y = Tie.function

      fun op --> _ = failing "Read.--> unsupported"

      val exn : Exn.t t = failing "Read.exn not yet implemented"
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list t = mkSequ (E#"[") (E#"]") ListOps.ops (getT t)
      fun vector t = mkSequ (L"#[") (E#"]") VectorOps.ops (getT t)

      fun array t = mkSequ (L"#(") (E#")") ArrayOps.ops (getT t)
      fun refc t = parens (I"ref" >>> map ref (getT t))

      val fixedInt  = mkInt FixedIntOps.ops
      val largeInt  = mkInt LargeIntOps.ops
      val largeWord = mkWord LargeWordOps.ops

      val bool = parens (alphaId >>= (fn "true"  => return true
                                       | "false" => return false
                                       | _       => zero))
      val char = parens (between (L"#\"") (E#"\"") (fromScan Char.scan))
      val int = mkInt IntOps.ops

      val string = let
         val satN = count o sat
         fun chars cs =
             E#"\\" >>= (fn _ => escape cs)
         <|> E#"\"" >>= (fn _ => return (implode (rev cs)))
         <|> sat Char.isPrint >>= (fn c => chars (c::cs))
         and escape cs =
             E#"^" >> sat Char.isPrint >>= (fn c => scan [#"^", c] cs)
         <|> satN Char.isDigit 3 >>= (fn ds => scan ds cs)
         <|> E#"u" >> satN Char.isHexDigit 4 >>= (fn ds => scan (#"u" :: ds) cs)
         <|> E#"U" >> satN Char.isHexDigit 8 >>= (fn ds => scan (#"U" :: ds) cs)
         <|> sat Char.isGraph >>= (fn c => scan [c] cs)
         <|> sat Char.isSpace >> skipSpaces >> E#"\\" >>= (fn _ =>
             chars cs)
         and scan c cs =
             case Char.scan List.getItem (#"\\" :: c)
              of SOME (c, []) => chars (c::cs)
               | _            => zero
      in
         parens (E#"\"" >> chars [])
      end

      val word = mkWord WordOps.ops

      val largeReal = mkReal LargeRealOps.ops
      val      real = mkReal      RealOps.ops

      val word8  = mkWord Word8Ops.ops
      val word32 = mkWord Word32Ops.ops
(*
      val word64 = mkWord Word64Ops.ops
*)

      fun hole () = undefined

      open Arg ReadRep)
end
