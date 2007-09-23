(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX indentation formatting option(s) *)

functor MkOpts (type 'a t) = struct
   type t = {intRadix  : StringCvt.radix t,
             wordRadix : StringCvt.radix t,
             realFmt   : StringCvt.realfmt t,
             maxDepth  : Int.t Option.t t,
             maxLength : Int.t Option.t t,
             maxString : Int.t Option.t t}
end

functor MapOpts (type 'a dom and 'a cod
                 val f : 'a dom -> 'a cod) = struct
   structure Dom = MkOpts (type 'a t = 'a dom)
   structure Cod = MkOpts (type 'a t = 'a cod)
   fun map (r : Dom.t) : Cod.t =
       {intRadix  = f (#intRadix  r),
        wordRadix = f (#wordRadix r),
        realFmt   = f (#realFmt   r),
        maxDepth  = f (#maxDepth  r),
        maxLength = f (#maxLength r),
        maxString = f (#maxString r)}
end

functor WithPretty (Arg : WITH_PRETTY_DOM) : PRETTY_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infixr 6 <^> <+>
   infixr 5 <$> <$$> </> <//>
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >|
   infixr 2 |<
   infix  1 >>=
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure Fixity = struct
      datatype t = ATOMIC | NONFIX | INFIXL of Int.t | INFIXR of Int.t
   end

   open Fixity

   fun mark f doc = (f, doc)

   open Prettier

   val parens       = (1, (lparen,   rparen))
   val hashParens   = (2, (txt "#(", rparen))
   val braces       = (1, (lbrace,   rbrace))
   val brackets     = (1, (lbracket, rbracket))
   val hashBrackets = (2, (txt "#[", rbracket))

   fun surround (n, p) = nest n o enclose p
   fun atomize (a, d) = if ATOMIC = a then d else surround parens d

   structure Fmt = struct
      structure Opts = MkOpts (type 'a t = 'a)

      datatype t = T of Opts.t

      val default =
          T {intRadix  = StringCvt.DEC,
             wordRadix = StringCvt.HEX,
             realFmt   = StringCvt.GEN NONE,
             maxDepth  = NONE,
             maxLength = NONE,
             maxString = NONE}

      structure RefOpts = MkOpts (Ref)

      datatype 'a opt =
         O of {get : Opts.t -> 'a,
               set : RefOpts.t -> 'a Ref.t,
               chk : 'a Effect.t}

      val notNeg = fn i => if i < 0 then raise Size else ()
      val notNegOpt = Option.app notNeg
      fun chkRealFmt fmt =
          if case fmt
              of StringCvt.SCI (SOME i) => i < 0
               | StringCvt.FIX (SOME i) => i < 0
               | StringCvt.GEN (SOME i) => i < 1
               | _                      => false
          then raise Size
          else ()

      val intRadix  = O {get = #intRadix,  set = #intRadix,  chk = ignore}
      val wordRadix = O {get = #wordRadix, set = #wordRadix, chk = ignore}
      val realFmt   = O {get = #realFmt,   set = #realFmt,   chk = chkRealFmt}
      val maxDepth  = O {get = #maxDepth,  set = #maxDepth,  chk = notNegOpt}
      val maxLength = O {get = #maxLength, set = #maxLength, chk = notNegOpt}
      val maxString = O {get = #maxString, set = #maxString, chk = notNegOpt}

      structure I = MapOpts (type 'a dom = 'a and 'a cod = 'a Ref.t val f = ref)
            and P = MapOpts (type 'a dom = 'a Ref.t and 'a cod = 'a val f = !)

      fun op & (T opts, (O {set, chk, ...}, v)) =
          (chk v
         ; case I.map opts
            of refOpts => (set refOpts := v ; T (P.map refOpts)))

      fun op := x = x

      fun ! (O {get, ...}) (T opts) = get opts
   end

   type c = {map : (HashUniv.t, Prettier.t Option.t) HashMap.t,
             cnt : Int.t Ref.t,
             fmt : Fmt.t}
   type v = {maxDepth : OptInt.t}
   datatype e = E of c * v
   type 'a t = e * 'a -> Fixity.t * Prettier.t
   type 'a p = e * 'a -> Prettier.t

   fun inj b a2b = b o Pair.map (id, a2b)

   val txt0b = txt "0b"
   val txt0o = txt "0o"
   val txt0w = txt "0w"
   val txt0wb = txt "0wb"
   val txt0wo = txt "0wo"
   val txt0wx = txt "0wx"
   val txt0x = txt "0x"
   val txtDots = txt "..."
   val txtFalse = txt "false"
   val txtFn = txt "#fn"
   val txtHash = txt "#"
   val txtHashDQuote = txt "#\""
   val txtNlBs = txt "\\n\\"
   val txtBsDots = txt "\\..."
   val txtTrue = txt "true"
   val txtUnit = txt "()"

   val ctorRef = Generics.C "ref"

   fun cyclic aT aP =
       case HashUniv.new {eq = op =, hash = Arg.hash aT}
        of (to, _) =>
           fn (e as E ({map, cnt, ...}, _), v) =>
              case to v
               of vD =>
                  case HashMap.find map vD
                   of SOME (SOME u) => (ATOMIC, u)
                    | SOME NONE => let
                         val u = txtHash <^>
                                 txt (Int.toString (cnt := !cnt + 1 ; !cnt))
                      in
                         HashMap.insert map (vD, SOME u)
                       ; (ATOMIC, u)
                      end
                    | NONE =>
                      (HashMap.insert map (vD, NONE)
                     ; case aP (e, v)
                        of (f, d) =>
                           (f,
                            lazy (fn () => case HashMap.find map vD
                                            of SOME (SOME u) => u <^> equals
                                             | _             => empty) <^> d))

   fun sequ style toSlice getItem aP (e as E ({fmt, ...}, _), a) = let
      fun lp (n, d, s) =
          case getItem s
           of NONE        => surround style d
            | SOME (a, s) => let
                 val d = d <^> comma
              in
                 if SOME 0 = n
                 then surround style (d <$> txtDots)
                 else lp (OptInt.- (n, SOME 1), d <$> group (aP (e, a)), s)
              end
      open Fmt
   in
      (ATOMIC,
       if SOME 0 = !maxLength fmt
       then surround style txtDots
       else case getItem (toSlice a)
             of NONE        => op <^> (#2 style)
              | SOME (a, s) =>
                lp (OptInt.- (!maxLength fmt, SOME 1), group (aP (e, a)), s))
   end

   val intPrefix =
    fn StringCvt.BIN => txt0b (* XXX HaMLet-S *)
     | StringCvt.OCT => txt0o (* XXX non-standard *)
     | StringCvt.DEC => empty
     | StringCvt.HEX => txt0x

   fun mkInt fmt (E ({fmt = Fmt.T {intRadix, ...}, ...}, _), i) =
       (ATOMIC, intPrefix intRadix <^> txt (fmt intRadix i))

   val wordPrefix =
    fn StringCvt.BIN => txt0wb (* XXX HaMLet-S *)
     | StringCvt.OCT => txt0wo (* XXX non-standard *)
     | StringCvt.DEC => txt0w
     | StringCvt.HEX => txt0wx

   fun mkWord fmt (E ({fmt = Fmt.T {wordRadix, ...}, ...}, _), w) =
       (ATOMIC, wordPrefix wordRadix <^> txt (fmt wordRadix w))

   fun mkReal fmt (E ({fmt = Fmt.T {realFmt, ...}, ...}, _), r) =
       (ATOMIC, txt (fmt realFmt r))

   fun depth aP (E (c, {maxDepth}), v) =
       if SOME 0 = maxDepth
       then (ATOMIC, txtDots)
       else aP (E (c, {maxDepth = OptInt.- (maxDepth, SOME 1)}), v)

   val exnHandler : Exn.t t Ref.t =
       ref (mark ATOMIC o txtHash <\ op <^> o txt o General.exnName o #2)
   fun regExn aP e2a =
       Ref.modify (fn exnHandler => fn (env, e) =>
                      case e2a e
                       of NONE   => exnHandler (env, e)
                        | SOME a => aP (env, a))
                  exnHandler

   fun iso' bP = inj bP o Iso.to

   structure PrettyRep = LayerRep
     (open Arg
      structure Rep = struct
         type 'a t = 'a t and 'a s = 'a t and ('a, 'k) p = 'a p
      end)

   open PrettyRep.This

   structure Pretty = struct
      type 'a monad = e -> 'a * e
      fun return a e = (a, e)
      fun (aM >>= a2bM) e = uncurry a2bM (aM e)

      fun getFmt (e as E ({fmt, ...}, _)) = (fmt, e)
      fun setFmt fmt (E ({cnt, map, ...}, v)) =
          ((), E ({cnt = cnt, fmt = fmt, map = map}, v))

      fun getRemDepth (e as E (_, {maxDepth})) = (maxDepth, e)
      fun setRemDepth remDepth =
          (Fmt.notNegOpt remDepth
         ; fn (E (c, _)) => ((), E (c, {maxDepth = remDepth})))

      structure Fixity = Fixity

      type 'a t = 'a -> (Fixity.t * Prettier.t) monad

      fun getPrinter aT =
          case getT aT
           of aP => fn a => fn e => (aP (e, a), e)
      fun setPrinter aP = mapT (const (Pair.fst o uncurry aP o Pair.swap))
      fun mapPrinter f t = setPrinter (f (getPrinter t)) t

      local
         fun mk con n cmpL cmpR =
             if n < 0 orelse 9 < n then raise Domain else
                fn c => case txt (Generics.Con.toString c) of c =>
                   fn (aT, bT) => case getT aT & getT bT of aP & bP =>
                      (mapS o const)
                         (fn (e, (a, b)) => let
                                val (aF, aS) = aP (e, a)
                                val (bF, bS) = bP (e, b)
                                val aS = if cmpL aF
                                         then surround parens aS
                                         else aS
                                val bS = if cmpR bF
                                         then surround parens bS
                                         else bS
                             in
                                (con n, aS <$> c </> bS)
                             end)
      in
         fun infixL n =
             mk INFIXL n
                (fn INFIXL l => l <  n | INFIXR r => r <= n | _ => false)
                (fn INFIXL l => l <= n | INFIXR r => r <= n | _ => false)
         fun infixR n =
             mk INFIXR n
                (fn INFIXL l => l <= n | INFIXR r => r <= n | _ => false)
                (fn INFIXL l => l <= n | INFIXR r => r <  n | _ => false)
      end
   end

   fun fmt t =
       case getT t
        of p => fn fmt => fn x =>
           group (#2 (p (E ({map = HashMap.new {eq = HashUniv.eq,
                                                hash = HashUniv.hash},
                             cnt = ref ~1,
                             fmt = fmt},
                            {maxDepth = Fmt.! Fmt.maxDepth fmt}),
                         x)))
   fun pretty t = fmt t Fmt.default
   fun show t = Prettier.render NONE o pretty t

   structure Open = LayerDepCases
     (fun iso        aT = iso' (getT aT)
      fun isoProduct aP = iso' (getP aP)
      fun isoSum     aS = iso' (getS aS)

      fun aP *` bP = let
         val aP = getP aP
         val bP = getP bP
      in
         fn (e, a & b) => aP (e, a) <^> comma <$> bP (e, b)
      end
      fun T t = group o #2 o getT t
      fun R l =
          case txt (Generics.Label.toString l)
           of l => fn aT => case T aT of aP => fn x =>
              group (nest 1 (l </> equals </> aP x))
      fun tuple aP = mark ATOMIC o surround parens o getP aP
      fun record aP = mark ATOMIC o surround braces o getP aP

      fun aS +` bS = let
         val aP = getS aS
         val bP = getS bS
      in
         fn (e, INL a) => aP (e, a)
          | (e, INR b) => bP (e, b)
      end
      fun unit _ = (ATOMIC, txtUnit)
      fun C0 c = const (ATOMIC, txt (Generics.Con.toString c))
      fun C1 c =
          case txt (Generics.Con.toString c)
           of c => fn aT => case getT aT of aP => fn ex =>
              (NONFIX, nest 1 (group (c <$> atomize (aP ex))))
      fun data aS = depth (getS aS)

      val Y = Tie.function

      fun exn ? = depth (!exnHandler) ?
      fun regExn0 c = case C0 c of uP => regExn uP o Pair.snd
      fun regExn1 c aT = case C1 c aT of aP => regExn aP o Pair.snd

      fun refc aT =
          cyclic (Arg.Open.refc ignore aT) o flip inj ! |< C1 ctorRef aT
      fun array aT =
          cyclic (Arg.Open.array ignore aT) |<
          sequ hashParens ArraySlice.full ArraySlice.getItem (T aT)
      fun vector aT =
          sequ hashBrackets VectorSlice.full VectorSlice.getItem (T aT)
      fun list aT = sequ brackets id List.getItem (T aT)

      fun op --> _ = const (ATOMIC, txtFn)

      local
         val toLit = txt o Substring.translate Char.toString
      in
         fun string (E ({fmt = Fmt.T {maxString, ...}, ...}, _), s) = let
            val cut = isSome maxString andalso valOf maxString < size s
            val suf = if cut then txtBsDots else empty
            val s = if cut
                    then Substring.substring (s, 0, valOf maxString)
                    else Substring.full s
         in
            mark ATOMIC o dquotes |< choice
               {wide = toLit s <^> suf,
                narrow = lazy (fn () =>
                   List.foldl1
                      (fn (x, s) => s <^> txtNlBs <$> backslash <^> x)
                      (List.map toLit (Substring.fields (#"\n" <\ op =) s)) <^>
                   suf)}
         end
      end

      fun bool (_, b) = (ATOMIC, if b then txtTrue else txtFalse)
      fun char (_, x) =
          (ATOMIC, txtHashDQuote <^> txt (Char.toString x) <^> dquote)
      val int  = mkInt Int.fmt
      val real = mkReal Real.fmt
      val word = mkWord Word.fmt

      val fixedInt = mkInt FixedInt.fmt
      val largeInt = mkInt LargeInt.fmt

      val largeReal = mkReal LargeReal.fmt
      val largeWord = mkWord LargeWord.fmt

      val word8  = mkWord Word8.fmt
      val word32 = mkWord Word32.fmt
      val word64 = mkWord Word64.fmt

      open Arg PrettyRep)
end
