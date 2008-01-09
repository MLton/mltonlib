(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SEQUENCE = sig
   type t
   structure Elem : T
   structure Pos : T
   val pos : t -> Pos.t
   val get : (Elem.t, t) Reader.t
end

signature MK_PARSEC_DOM = sig
   structure Sequence : SEQUENCE
end

signature PARSEC = sig
   include MK_PARSEC_DOM

   include ETAEXP'
   include MONADP where type 'a monad = 'a etaexp

   type 'a t = 'a etaexp

   val parse : 'a t -> Sequence.t -> (Sequence.Pos.t, 'a * Sequence.t) Sum.t
   val fromScan :
       ((Sequence.Elem.t, Sequence.t) Reader.t -> ('a, Sequence.t) Reader.t) -> 'a t
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
   where type Sequence.Pos.t  = Arg.Sequence.Pos.t =
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
   type 'a etaexp_dom = Sequence.t
   type msg = Sequence.Pos.t
   datatype 'a reply =
      OK   of 'a * 'a etaexp_dom * msg
    | FAIL of msg
   datatype 'a etaexp_cod =
      EATEN of 'a reply
    | TASTE of 'a reply Thunk.t
    | EMPTY of 'a reply
   type 'a etaexp = 'a etaexp_dom -> 'a etaexp_cod
   type 'a t = 'a etaexp

   val get = Sequence.get
   val pos = Sequence.pos

   fun parse p s =
       case case p s
             of EMPTY r  => r
              | EATEN r  => r
              | TASTE th => th ()
        of FAIL p       => INL p
         | OK (x, s, _) => INR (x, s)

   fun fromReader reader s =
       case reader s
        of SOME (x, s) => EATEN (OK (x, s, pos s))
         | NONE        => EMPTY (FAIL (pos s))

   fun fromScan scan = fromReader (scan Sequence.get)

   fun merge m =
    fn OK (x, s, _) => OK (x, s, m)
     | FAIL _       => FAIL m

   fun bindSome m =
    fn EMPTY r  => merge m r
     | EATEN r  => r
     | TASTE th => th ()

   fun replyNone m =
    fn EMPTY r => EMPTY (merge m r)
     | other   => other

   fun return x s = EMPTY (OK (x, s, pos s))

   fun (xM >>= x2yM) s =
       case xM s
        of EATEN (FAIL m)       => EATEN (FAIL m)
         | EATEN (OK (x, s, m)) => TASTE (fn () => bindSome m (x2yM x s))
         | TASTE th             => TASTE (fn () =>
                                      case th ()
                                       of FAIL e       => FAIL e
                                        | OK (x, s, m) => bindSome m (x2yM x s))
         | EMPTY (FAIL m)       => EMPTY (FAIL m)
         | EMPTY (OK (x, s, m)) => replyNone m (x2yM x s)

   fun zero s = EMPTY (FAIL (pos s))

   fun (p <|> q) s =
       case p s
        of EMPTY (FAIL m) => replyNone m (q s)
         | other          => other

   fun guess p s =
       case p s
        of EMPTY r        => EMPTY r
         | EATEN (FAIL _) => EMPTY (FAIL (pos s))
         | EATEN (OK r)   => EATEN (OK r)
         | TASTE th       => case th ()
                              of FAIL _ => EMPTY (FAIL (pos s))
                               | result => EATEN result

   fun elem s =
       case get s
        of NONE        => EMPTY (FAIL (pos s))
         | SOME (c, s) => EATEN (OK (c, s, pos s))

   fun drop p s = let
      fun done f s = f (OK ((), s, pos s))
      fun some (c, s') s = if p c then lp s' else done EATEN s
      and body f s =
          case get s
           of NONE    => done f s
            | SOME cs => some cs s
      and lp s = body EATEN s
   in
      body EMPTY s
   end

   fun sat p s =
       case get s
        of NONE         => EMPTY (FAIL (pos s))
         | SOME (c, s') =>
           EATEN (if p c then OK (c, s', pos s') else FAIL (pos s))

   fun take p = let
      fun done s =
       fn [] => EMPTY (OK ([], s, pos s))
        | cs => EATEN (OK (rev cs, s, pos s))
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
        of EATEN (OK (x, _, m)) => EATEN (OK (x, s, m))
         | EATEN (FAIL m)       => EATEN (FAIL m)
         | EMPTY (OK (x, _, m)) => EMPTY (OK (x, s, m))
         | EMPTY (FAIL m)       => EMPTY (FAIL m)
         | TASTE th             => case th ()
                                    of OK (x, _, m) => EATEN (OK (x, s, m))
                                     | FAIL m       => EATEN (FAIL m)

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

   infix 1 >> >>>

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
      end)
   open Parsec

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

   val spaces = drop Char.isSpace

   fun l >>> r = l >> spaces >> r

   fun wrap p =
       L"(" >>> eta wrap p >>= (fn x => L")" >>> return x) <|>
       p >>= (fn x => spaces >> return x)

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
   val longId = map op :: (shortId >>* ^* (L"." >> shortId))

   val numLabel =
       map (implode o op ::)
           (guess (sat (Char.inRange (#"1", #"9"))) >>* take Char.isDigit)
   val label = numLabel <|> shortId

   fun mkSequ pre suf (Ops.S {fromList, ...}) p = let
      val pre = L pre val suf = L suf val sep = L","
      fun aft xs = suf >>= (fn () => return (fromList (rev xs))) <|>
                   sep >>> eta bef xs
      and bef xs = p >>= (fn x => aft (x::xs))
   in
      wrap (pre >>> (suf >>= (fn () => return (fromList [])) <|> bef []))
   end

   fun mkReal (Ops.R {scan, ...} : ('r, 'w, Sequence.t) Ops.r) : 'r t =
       wrap (fromScan scan)

   fun mkScalar scan mk = wrap (mk (fromScan o scan))

   fun mkWord (Ops.W {scan, ...} : ('w, Sequence.t) Ops.w) : 'w t =
       mkScalar scan (fn p => L"0w" >> (L"x" >> p HEX <|>
                                        L"o" >> p OCT <|>
                                        L"b" >> p BIN <|>
                                                p DEC))

   fun mkInt (Ops.I {scan, ...} : ('i, Sequence.t) Ops.i) : 'i t =
       mkScalar scan (fn p => peek (L"~0x" <|> L"0x") >> p HEX <|>
                              peek (L"~0o" <|> L"0o") >> p OCT <|>
                              peek (L"~0b" <|> L"0b") >> p BIN <|>
                                                         p DEC)

   datatype 'a p =
      INP of (String.t * Univ.t t) List.t *
             (Univ.t Option.t ArraySlice.t -> 'a * Univ.t Option.t ArraySlice.t)

   structure ReadRep = LayerRep
     (open Arg
      structure Rep = struct
         type  'a      t = 'a t
         type  'a      s = String.t -> 'a t Option.t
         type ('a, 'k) p = 'a p
      end)

   open ReadRep.This

   fun reader t =
       case getT t
        of pA => fn rC => fn s =>
           case Univ.Iso.new ()
            of (to, from) =>
               Sum.map (from, id)
                       (parse (spaces >> pA)
                              (Reader.mapState (from, to) rC, to s))

   fun read t =
       (fn INR (x, _) => x
         | INL s => let
              val (str, pos, len) = Substring.base s
              val size = len + pos
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
           end) o
       reader t Substring.getc o
       Substring.full

   structure Open = LayerCases
     (fun iso bP (_, b2a) = map b2a bP
      fun isoProduct (INP (lps, fromSlice)) (_, b2a) =
          INP (lps, Pair.map (b2a, id) o fromSlice)
      fun isoSum bS (_, b2a) s = Option.map (map b2a) (bS s)

      fun op *` (INP (ls, la), INP (rs, ra)) =
          INP (ls @ rs,
               fn ars =>
                  case la ars
                   of (l, ars) =>
                      case ra ars
                       of (r, ars) => (l & r, ars))
      fun T t =
          case Univ.Iso.new ()
           of (to, from) =>
              INP ([("", map to t)],
                   fn ars => case ArraySlice.getItem ars
                              of SOME (SOME u, ars) => (from u, ars)
                               | _                  => fail "impossible")
      fun R l t =
          case Univ.Iso.new ()
           of (to, from) =>
              INP ([(Generics.Label.toString l, map to t)],
                   fn ars => case ArraySlice.getItem ars
                              of SOME (SOME u, ars) => (from u, ars)
                               | _                  => fail "impossible")
      fun tuple (INP (lps, fromSlice)) = let
         val ps = List.map #2 lps
         val n = length ps
         fun lp a i =
          fn []    => L")" >> return (#1 (fromSlice (ArraySlice.full a)))
           | p::ps => p >>= (fn x =>
                      (Array.update (a, i, SOME x)
                     ; (if null ps
                        then return ()
                        else L",") >>> lp a (i+1) ps))
      in
         L"(" >>> wrap (lp (Array.array (n, NONE)) 0 ps)
      end
      fun record (INP (lps, fromSlice)) = let
         val n = length lps
         fun lp a =
          fn 0 => L"}" >> return (#1 (fromSlice (ArraySlice.full a)))
           | n => label >>= (fn l =>
                  case List.findi (l <\ op = o #1 o #2) lps
                   of NONE             => zero
                    | SOME (i, (_, p)) =>
                      if isSome (Array.sub (a, i))
                      then zero
                      else spaces >> symbolicId >>= (fn "=" => return ()
                                                      | _   => zero) >>>
                           p >>= (fn x =>
                           (Array.update (a, i, SOME x)
                          ; if n <= 1
                            then lp a 0
                            else L"," >>> lp a (n-1))))
      in
         wrap (L"{" >>> (fn ? => lp (Array.array (n, NONE)) n ?))
      end

      fun op +` (l, r) s =
          case l s
           of SOME l => SOME (map INL l)
            | NONE   => Option.map (map INR) (r s)
      val unit = L"(" >>> wrap (L")")
      fun C0 c s = if s = Generics.Con.toString c then SOME spaces else NONE
      fun C1 c t s =
          if s = Generics.Con.toString c then SOME (spaces >> t) else NONE
      fun data t =
          wrap (longId >>= (fn s => case t (String.concatWith "." s)
                                     of NONE   => zero
                                      | SOME p => p))

      val Y = Tie.function

      fun op --> _ = failing "Read.--> unsupported"

      val exn : Exn.t t = failing "Read.exn not yet implemented"
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list t = mkSequ "[" "]" ListOps.ops t
      fun vector t = mkSequ "#[" "]" VectorOps.ops t

      fun array _ = failing "Read.array not yet implemented"
      fun refc _ = failing "Read.refc not yet implemented"

      val fixedInt  = mkInt FixedIntOps.ops
      val largeInt  = mkInt LargeIntOps.ops
      val largeWord = mkWord LargeWordOps.ops

      val bool =
          wrap (alphaId >>= (fn "true"  => return true
                              | "false" => return false
                              | _       => zero))
      val char =
          wrap (L"#\"" >> fromScan Char.scan >>= (fn c => L"\"" >> return c))
      val int = mkInt IntOps.ops
      val string = let
         fun finish cs stm =
             case String.scan List.getItem cs
              of NONE           => NONE
               | SOME (str, []) => SOME (str, stm)
               | SOME _         => NONE
         fun ord cs s =
             case Sequence.get s
              of NONE            => NONE
               | SOME (#"\"", _) => finish (rev cs) s
               | SOME (#"\\", s) => esc (#"\\"::cs) s
               | SOME (c,     s) => ord (c::cs) s
         and esc cs s =
             case Sequence.get s
              of NONE           => NONE
               | SOME (#"^", s) => hat (#"^"::cs) s
               | SOME (c,    s) =>
                 if Char.isSpace c then fmt (c::cs) s
                 else if Char.isDigit c then dec 2 (c::cs) s
                 else ord (c::cs) s
         and fmt cs s =
             case Sequence.get s
              of NONE            => NONE
               | SOME (#"\\", s) => ord (#"\\"::cs) s
               | SOME (c,     s) =>
                 if Char.isSpace c then fmt (c::cs) s else NONE
         and dec n cs s =
             if 0 = n
             then ord cs s
             else case Sequence.get s
                   of NONE        => NONE
                    | SOME (c, s) =>
                      if Char.isDigit c then dec (n-1) (c::cs) s else NONE
         and hat cs s =
             case Sequence.get s
              of NONE        => NONE
               | SOME (c, s) => ord (c::cs) s
      in
         wrap (L"\"" >> fromReader (ord []) >>= (fn s => L"\"" >> return s))
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
