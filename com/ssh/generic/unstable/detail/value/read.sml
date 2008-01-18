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
   infix  1 orElse >>= >>& >>*
   infix  0 & <|>
   infixr 0 -->
   (* SML/NJ workaround --> *)

   infix 1 << >> <<< >>>

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

   fun E c = fromReader (fn s => case Sequence.get s
                                  of NONE         => NONE
                                   | SOME (c', s) =>
                                     if c' = c then SOME ((), s) else NONE)
   fun L l = fromReader let
      fun lp i s =
          if i = size l
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

   fun ignored 0 = drop Char.isSpace >> (L"(*" >> eta ignored 1 <|> return ())
     | ignored n = L"*)" >> eta ignored (n-1) <|>
                   L"(*" >> eta ignored (n+1) <|>
                   elem  >> eta ignored n

   val ignored = ignored 0

   fun l << r = l >>= (fn l => r >> return l)
   fun l >>> r = l >> ignored >> r
   fun l <<< r = l >>= (fn l => ignored >> r >> return l)

   fun parens p = guess (E#"(" >>> eta parens p) <<< E#")" <|> p

   datatype radix = datatype StringCvt.radix

   val alphaId =
       map (implode o op ::)
           (sat Char.isAlpha >>*
            take (fn c => Char.isAlpha c
                          orelse Char.isDigit c
                          orelse #"'" = c orelse #"_" = c))
   val symbolicId =
       take (Char.contains "!#$%&*+-/:<=>?@\\^`|~") >>=
            (fn [] => zero | cs => return (implode cs))

   val shortId = alphaId <|> symbolicId
   val longId = map op :: (shortId >>* ^* (E#"." >> shortId))
   fun I s = shortId >>= (fn i => if i = s then return () else zero)

   val numLabel =
       map (implode o op ::)
           (sat (Char.inRange (#"1", #"9")) >>* take Char.isDigit)
   val label = numLabel <|> shortId

   fun mkSequ pre suf (Ops.S {fromList, ...}) p = let
      fun fin xs () = return (fromList (rev xs))
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

   datatype 'a p =
      INP of (String.t * Univ.t t) List.t *
             (Univ.t Option.t ArraySlice.t -> 'a * Univ.t Option.t ArraySlice.t)

   fun F l t =
       case Univ.Iso.new ()
        of (to, from) =>
           INP ([(l, map to t)],
                fn ars => case ArraySlice.getItem ars
                           of SOME (SOME u, ars) => (from u, ars)
                            | _                  => fail "impossible")

   fun C c p s = if s = Generics.Con.toString c then SOME p else NONE

   structure ReadRep = LayerRep
     (open Arg
      structure Rep = struct
         type  'a      t = 'a t
         type  'a      s = String.t -> 'a t Option.t
         type ('a, 'k) p = 'a p
      end)

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
         val str = StringSequence.string s
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
              if StringSequence.pos s = size (StringSequence.string s)
              then x
              else error s
            | INL s => error s) o
          reader' (getT t << ignored)
                  StringSequence.get o
          StringSequence.full
   end

   structure Open = LayerCases
     (fun iso bP (_, b2a) = map b2a bP
      fun isoProduct (INP (lps, fromSlice)) (_, b2a) =
          INP (lps, Pair.map (b2a, id) o fromSlice)
      fun isoSum bS (_, b2a) = Option.map (map b2a) o bS

      fun op *` (INP (ls, la), INP (rs, ra)) =
          INP (ls @ rs,
               fn ars =>
                  case la ars
                   of (l, ars) =>
                      case ra ars
                       of (r, ars) => (l & r, ars))
      fun T t = F "" t
      fun R l = F (Generics.Label.toString l)
      fun tuple (INP (lps, fromSlice)) = let
         val ps = List.map #2 lps
         val n = length ps
         fun lp a i =
          fn []    => E#")" >> return (#1 (fromSlice (ArraySlice.full a)))
           | p::ps => p >>= (fn x =>
                      (Array.update (a, i, SOME x)
                     ; (if null ps
                        then ignored
                        else ignored >> E#"," >> ignored) >> lp a (i+1) ps))
      in
         E#"(" >>> parens (fn ? => lp (Array.array (n, NONE)) 0 ps ?)
      end
      fun record (INP (lps, fromSlice)) = let
         val n = length lps
         fun lp a =
          fn 0 => E#"}" >> return (#1 (fromSlice (ArraySlice.full a)))
           | n => label >>= (fn l =>
                  case List.findi (l <\ op = o #1 o #2) lps
                   of NONE             => zero
                    | SOME (i, (_, p)) =>
                      if isSome (Array.sub (a, i))
                      then zero
                      else ignored >> I"=" >>> p >>= (fn x =>
                           (Array.update (a, i, SOME x)
                          ; if n <= 1
                            then ignored >> lp a 0
                            else ignored >> E#"," >>> lp a (n-1))))
      in
         parens (E#"{" >>> (fn ? => lp (Array.array (n, NONE)) n ?))
      end

      fun op +` (l, r) s =
          case l s
           of SOME l => SOME (map INL l)
            | NONE   => Option.map (map INR) (r s)
      val unit = E#"(" >>> parens (E#")")
      fun C0 c = C c (return ())
      fun C1 c t = C c (ignored >> t)
      fun data t =
          parens (parens longId >>= (fn s => case t (String.concatWith "." s)
                                              of NONE   => zero
                                               | SOME p => p))

      val Y = Tie.function

      fun op --> _ = failing "Read.--> unsupported"

      val exn : Exn.t t = failing "Read.exn not yet implemented"
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list t = mkSequ (E#"[") (E#"]") ListOps.ops t
      fun vector t = mkSequ (L"#[") (E#"]") VectorOps.ops t

      fun array t = mkSequ (L"#(") (E#")") ArrayOps.ops t
      fun refc t = parens (I"ref" >>> map ref t)

      val fixedInt  = mkInt FixedIntOps.ops
      val largeInt  = mkInt LargeIntOps.ops
      val largeWord = mkWord LargeWordOps.ops

      val bool = parens (alphaId >>= (fn "true"  => return true
                                       | "false" => return false
                                       | _       => zero))
      val char = parens (L"#\"" >> fromScan Char.scan << E#"\"")
      val int = mkInt IntOps.ops

      val string = let
         fun satN p n = let
            fun lp cs =
             fn 0 => return (rev cs)
              | n => sat p >>= (fn c => lp (c::cs) (n-1))
         in
            lp [] n
         end
         fun chars cs =
             E#"\\" >>= (fn () => escape cs)
         <|> E#"\"" >>= (fn () => return (implode (rev cs)))
         <|> sat Char.isPrint >>= (fn c => chars (c::cs))
         and escape cs =
             E#"^" >> sat Char.isPrint >>= (fn c => scan [#"^", c] cs)
         <|> satN Char.isDigit 3 >>= (fn ds => scan ds cs)
         <|> E#"u" >> satN Char.isHexDigit 4 >>= (fn ds => scan (#"u" :: ds) cs)
         <|> E#"U" >> satN Char.isHexDigit 8 >>= (fn ds => scan (#"U" :: ds) cs)
         <|> sat Char.isGraph >>= (fn c => scan [c] cs)
         <|> sat Char.isSpace >> drop Char.isSpace >> E#"\\" >>= (fn () =>
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
