(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature MK_PARSEC_DOM = sig
   structure Sequence : SEQUENCE
   structure State : T
end

signature PARSEC = sig
   include MK_PARSEC_DOM

   include ETAEXP'
   include MONADP where type 'a monad = 'a etaexp

   type 'a t = 'a etaexp

   val parse : 'a t -> Sequence.t * State.t
               -> (Sequence.Pos.t, 'a * (Sequence.t * State.t)) Sum.t

   val getState : State.t t
   val setState : State.t -> Unit.t t

   val fromScan : ((Sequence.Elem.t, Sequence.t) Reader.t
                   -> ('a, Sequence.t) Reader.t) -> 'a t
   val fromReader : ('a, Sequence.t) Reader.t -> 'a t

   val guess : 'a t UnOp.t

   val elem : Sequence.Elem.t t
   val drop : Sequence.Elem.t UnPr.t -> Unit.t t
   val sat : Sequence.Elem.t UnPr.t -> Sequence.Elem.t t
   val take : Sequence.Elem.t UnPr.t -> Sequence.Elem.t List.t t

   val peek : 'a t UnOp.t
   val ^* : 'a t -> 'a List.t t
end

functor MkParsec (Arg : MK_PARSEC_DOM) :> PARSEC
   where type Sequence.t      = Arg.Sequence.t
   where type Sequence.Elem.t = Arg.Sequence.Elem.t
   where type Sequence.Pos.t  = Arg.Sequence.Pos.t
   where type State.t         = Arg.State.t =
struct
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
   infix  1 orElse >>=
   infix  0 & <|>
   infixr 0 -->
   (* SML/NJ workaround --> *)

   open Arg
   type 'a etaexp_dom = Sequence.t * State.t
   type msg = Sequence.Pos.t
   datatype 'a reply =
      OK   of 'a * 'a etaexp_dom * msg
    | FAIL of msg
   datatype 'a etaexp_cod =
      TASTE of 'a reply Thunk.t
    | EMPTY of 'a reply
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod
   type 'a t = 'a etaexp

   fun get (s, t) = Option.map (fn (e, s) => (e, (s, t))) (Sequence.get s)
   fun pos (s, _) = Sequence.pos s

   fun getState (s, t) = EMPTY (OK (t, (s, t), Sequence.pos s))
   fun setState t (s, _) = EMPTY (OK ((), (s, t), Sequence.pos s))

   fun taste r = TASTE (const r)

   fun parse p s =
       case case p s
             of EMPTY r  => r
              | TASTE th => th ()
        of FAIL p       => INL p
         | OK (x, s, _) => INR (x, s)

   fun fromReader reader (s, t) =
       case reader s
        of SOME (x, s) => taste (OK (x, (s, t), Sequence.pos s))
         | NONE        => EMPTY (FAIL (Sequence.pos s))

   fun fromScan scan = fromReader (scan Sequence.get)

   fun merge m =
    fn OK (x, s, _) => OK (x, s, m)
     | FAIL _       => FAIL m

   fun bindSome m =
    fn EMPTY r  => merge m r
     | TASTE th => th ()

   fun replyNone m =
    fn EMPTY r => EMPTY (merge m r)
     | other   => other

   fun return x s = EMPTY (OK (x, s, pos s))

   fun (xM >>= x2yM) s =
       case xM s
        of EMPTY (FAIL m)       => EMPTY (FAIL m)
         | EMPTY (OK (x, s, m)) => replyNone m (x2yM x s)
         | TASTE th             =>
           TASTE (fn () => case th ()
                            of FAIL e       => FAIL e
                             | OK (x, s, m) => bindSome m (x2yM x s))

   fun zero s = EMPTY (FAIL (pos s))

   fun (p <|> q) s =
       case p s
        of EMPTY (FAIL m) => replyNone m (q s)
         | other          => other

   fun guess p s =
       case p s
        of EMPTY r  => EMPTY r
         | TASTE th => case th ()
                        of FAIL _ => EMPTY (FAIL (pos s))
                         | result => taste result

   fun elem s =
       case get s
        of NONE        => EMPTY (FAIL (pos s))
         | SOME (c, s) => taste (OK (c, s, pos s))

   fun drop p s = let
      fun done f s = f (OK ((), s, pos s))
      fun some (c, s') s = if p c then lp s' else done taste s
      and body f s =
          case get s
           of NONE    => done f s
            | SOME cs => some cs s
      and lp s = body taste s
   in
      body EMPTY s
   end

   fun sat p s =
       case get s
        of NONE         => EMPTY (FAIL (pos s))
         | SOME (c, s') =>
           taste (if p c then OK (c, s', pos s') else FAIL (pos s))

   fun take p = let
      fun done s =
       fn [] => EMPTY (OK ([], s, pos s))
        | cs => taste (OK (rev cs, s, pos s))
      fun lp cs s =
          case get s
           of NONE => done s cs
            | SOME (c, s') =>
              if p c
              then lp (c::cs) s'
              else done s cs
   in
      lp []
   end

   fun peek p s =
       case p s
        of EMPTY (OK (x, _, m)) => EMPTY (OK (x, s, m))
         | EMPTY (FAIL m)       => EMPTY (FAIL m)
         | TASTE th             => case th ()
                                    of OK (x, _, m) => taste (OK (x, s, m))
                                     | FAIL m       => taste (FAIL m)

   fun ^* p = p >>= (fn x => ^* p >>= (fn xs => return (x::xs))) <|> return []

   structure Monad = MkMonadP
     (type 'a monad = 'a t
      val return = return
      val op >>= = op >>=
      val zero = zero
      val op <|> = op <|>)

   open Monad
end

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
           (guess (sat Char.isAlpha) >>*
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
           (guess (sat (Char.inRange (#"1", #"9"))) >>* take Char.isDigit)
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
         (* Note that this is only an approximate parser for string literals. *)
         fun chars cs =
             elem >>= (fn #"\\" => escape cs
                        | #"\"" => (case String.scan List.getItem (rev cs)
                                     of SOME (s, []) => return s
                                      | _            => zero)
                        | c     => if Char.isPrint c
                                   then chars (c :: cs)
                                   else zero)
         and escape cs =
             elem >>= (fn c => if #"^" = c then
                                  sat Char.isPrint >>= (fn c =>
                                  chars (c :: #"^" :: #"\\" :: cs))
                               else if Char.isSpace c then
                                  drop Char.isSpace >> E#"\\" >> chars cs
                               else if Char.isPrint c then
                                  chars (c :: #"\\" :: cs)
                               else
                                  zero)
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
