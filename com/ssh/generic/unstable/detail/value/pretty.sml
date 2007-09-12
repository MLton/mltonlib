(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX pretty printing could use some tuning *)
(* XXX parameters for pretty printing? *)
(* XXX parameters for depth, length, etc... for showing only partial data *)

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
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   datatype f = ATOMIC | NONFIX

   fun mark f doc = (f, doc)

   open Prettier

   fun surround (n, p) = mark ATOMIC o group o nest n o enclose p
   fun atomize (a, d) = if ATOMIC = a then d else parens d

   val parens       = (1, (lparen,   rparen))
   val hashParens   = (2, (txt "#(", rparen))
   val braces       = (1, (lbrace,   rbrace))
   val brackets     = (1, (lbracket, rbracket))
   val hashBrackets = (2, (txt "#[", rbracket))

   type e = (HashUniv.t, Prettier.t Option.t) HashMap.t * Int.t Ref.t
   type 'a t = e * 'a -> f * Prettier.t
   type 'a p = e * 'a -> Prettier.t

   fun inj b a2b = b o Pair.map (id, a2b)

   val txt0wx = txt "0wx"
   val txtFn = txt "#fn"
   val txtHash = txt "#"
   val txtHashDQuote = txt "#\""
   val txtNlBs = txt "\\n\\"
   val txtUnit = txt "()"

   val ctorRef = Generics.C "ref"

   fun cyclic aT aP =
       case HashUniv.new {eq = op =, hash = Arg.hash aT}
        of (to, _) =>
           fn ((e, c), v) =>
              case to v
               of vD =>
                  case HashMap.find e vD
                   of SOME (SOME u) => (ATOMIC, u)
                    | SOME NONE => let
                         val u = txtHash <^> txt (Int.toString (c := !c + 1 ; !c))
                      in
                         HashMap.insert e (vD, SOME u)
                       ; (ATOMIC, u)
                      end
                    | NONE =>
                      (HashMap.insert e (vD, NONE)
                     ; case aP ((e, c), v)
                        of (f, d) =>
                           (f,
                            lazy (fn () => case HashMap.find e vD
                                            of SOME (SOME u) => u <^> equals
                                             | _             => empty) <^> d))
                      
   fun sequ style toSlice getItem aP (e, a) = let
      fun lp (d, s) =
          case getItem s
           of NONE        => surround style d
            | SOME (a, s) => lp (d <^> comma <$> aP (e, a), s)
   in
      case getItem (toSlice a)
       of NONE        => (ATOMIC, op <^> (#2 style))
        | SOME (a, s) => lp (aP (e, a), s)
   end

   fun mk toString : 'a t = mark ATOMIC o txt o toString o Pair.snd
   fun mkWord toString : 'a t =
       mark ATOMIC o txt0wx <\ op <^> o txt o toString o Pair.snd

   val exnHandler : Exn.t t Ref.t =
       ref (mark ATOMIC o txtHash <\ op <^> o txt o General.exnName o #2)
   fun regExn aP e2a =
       Ref.modify (fn exnHandler => fn (env, e) =>
                      case e2a e
                       of NONE   => exnHandler (env, e)
                        | SOME a => aP (env, a))
                  exnHandler

   fun iso' bP = inj bP o Iso.to

   structure Pretty = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = struct
         type 'a t = 'a t
         type 'a s = 'a t
         type ('a, 'k) p = 'a p
      end)

   open Pretty.This

   fun pretty t =
       case getT t
        of p => fn x => #2 (p ((HashMap.new {eq = HashUniv.eq,
                                             hash = HashUniv.hash}, ref ~1), x))
   fun show t = Prettier.render NONE o pretty t

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Pretty

      fun iso        aT = iso' (getT aT)
      fun isoProduct aP = iso' (getP aP)
      fun isoSum     aS = iso' (getS aS)

      fun aP *` bP = let
         val aP = getP aP
         val bP = getP bP
      in
         fn (e, a & b) => aP (e, a) <^> comma <$> bP (e, b)
      end
      fun T t = #2 o getT t
      fun R l =
          case txt (Generics.Label.toString l)
           of l => fn aT => case T aT of aP => fn x =>
              group (nest 1 (l </> equals </> aP x))
      fun tuple aP = surround parens o getP aP
      fun record aP = surround braces o getP aP

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
      val data = getS

      val Y = Tie.function

      fun exn ? = !exnHandler ?
      fun regExn0 c = case C0 c of uP => regExn uP o Pair.snd
      fun regExn1 c aT = case C1 c aT of aP => regExn aP o Pair.snd

      fun refc aT = cyclic (Arg.refc ignore aT) o flip inj ! |< C1 ctorRef aT
      fun array aT =
          cyclic (Arg.array ignore aT) |<
          sequ hashParens ArraySlice.full ArraySlice.getItem (T aT)
      fun vector aT =
          sequ hashBrackets VectorSlice.full VectorSlice.getItem (T aT)
      fun list aT = sequ brackets id List.getItem (T aT)

      fun op --> _ = const (ATOMIC, txtFn)

      local
         val toLit = txt o String.toString
      in
         fun string (_, s) =
             mark ATOMIC o group o dquotes |< choice
                {wide = toLit s,
                 narrow = lazy (fn () =>
                    List.foldl1
                       (fn (x, s) => s <^> txtNlBs <$> backslash <^> x)
                       (List.map toLit (String.fields (#"\n" <\ op =) s)))}
      end

      val bool = mk Bool.toString
      fun char (_, x) =
          (ATOMIC, txtHashDQuote <^> txt (Char.toString x) <^> dquote)
      val int  = mk Int.toString
      val real = mk Real.toString
      val word = mkWord Word.toString

      val fixedInt = mk FixedInt.toString
      val largeInt = mk LargeInt.toString

      val largeReal = mk LargeReal.toString
      val largeWord = mkWord LargeWord.toString

      val word8  = mkWord Word8.toString
      val word32 = mkWord Word32.toString
      val word64 = mkWord Word64.toString)

   open Layered
end
