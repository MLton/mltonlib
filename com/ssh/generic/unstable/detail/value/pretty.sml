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

   local
      open Prettier
      type u = Bool.t * t
      fun nonAtomic doc = (false, doc)
      val uop : t UnOp.t -> u UnOp.t = id <\ Pair.map
      val bop : t BinOp.t -> u BinOp.t =
          fn f => nonAtomic o f o Pair.map (Sq.mk Pair.snd)
   in
      type u = u

      fun atomic doc = (true,  doc)

      val parens       = (1, (lparen,   rparen))
      val hashParens   = (2, (txt "#(", rparen))
      val braces       = (1, (lbrace,   rbrace))
      val brackets     = (1, (lbracket, rbracket))
      val hashBrackets = (2, (txt "#[", rbracket))

      val comma  = atomic comma
      val equals = atomic equals

      val txt = atomic o txt
      fun surround (n, p) = atomic o group o nest n o enclose p o Pair.snd
      fun atomize (d as (a, _)) = if a then d else surround parens d
      val punctuate = fn (_, s) => punctuate s o List.map Pair.snd
      val fill = fn ? => nonAtomic (vsep ?)
      val group = uop group
      val nest = uop o nest
      val op <^> = fn ((al, dl), (ar, dr)) => (al andalso ar, dl <^> dr)
      val op <$> = bop op <$>
      val op </> = bop op </>
   end

   type e = (HashUniv.t, Prettier.t Option.t) HashMap.t * Int.t Ref.t
   type 'a t = e * 'a -> u

   fun inj b a2b = b o Pair.map (id, a2b)

   val txtFn = txt "#fn"

   val ctorRef = Generics.C "ref"

   fun cyclic aT aP =
       case HashUniv.new {eq = op =, hash = Arg.hash aT}
        of (to, _) =>
           fn ((e, c), v) =>
              case to v
               of vD =>
                  case HashMap.find e vD
                   of SOME (SOME u) => atomic u
                    | SOME NONE => let
                         val u = Prettier.txt ("#"^Int.toString (c := !c + 1 ; !c))
                      in
                         HashMap.insert e (vD, SOME u)
                       ; atomic u
                      end
                    | NONE =>
                      (HashMap.insert e (vD, NONE)
                     ; (true,
                        let open Prettier in
                           lazy (fn () => case HashMap.find e vD
                                           of SOME (SOME u) => u <^> equals
                                            | _             => empty)
                        end) <^>
                       aP ((e, c), v))

   fun sequ style toL t (e, a) =
       surround style o fill o punctuate comma o List.map (curry t e) |< toL a

   fun mk toS : 'a t = txt o toS o Pair.snd
   fun enc l r toS x = concat [l, toS x, r]
   fun mkWord toString = mk ("0wx" <\ op ^ o toString)

   val exnHandler : Exn.t t Ref.t =
       ref (txt o "#" <\ op ^ o General.exnName o #2)
   fun regExn aP e2a =
       Ref.modify (fn exnHandler => fn (env, e) =>
                      case e2a e
                       of NONE   => exnHandler (env, e)
                        | SOME a => aP (env, a))
                  exnHandler

   fun iso' getX bX = inj (getX bX) o Iso.to

   structure Pretty = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Pretty.This

   fun layout t =
       case getT t
        of p => fn x => #2 (p ((HashMap.new {eq = HashUniv.eq,
                                             hash = HashUniv.hash}, ref ~1), x))
   fun pretty m t = Prettier.pretty m o layout t
   fun show t = pretty NONE t

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Pretty

      fun iso        ? = iso' getT ?
      fun isoProduct ? = iso' getP ?
      fun isoSum     ? = iso' getS ?

      fun aP *` bP = let
         val aP = getP aP
         val bP = getP bP
      in
         fn (e, a & b) => aP (e, a) <^> comma <$> bP (e, b)
      end
      val T = getT
      fun R l =
          case txt (Generics.Label.toString l)
           of l =>
              fn aT => case getT aT
                        of aP => fn ? => group (nest 1 (l </> equals </> aP ?))
      fun tuple aP = surround parens o getP aP
      fun record aP = surround braces o getP aP

      fun aS +` bS = let
         val aP = getS aS
         val bP = getS bS
      in
         fn (e, INL a) => aP (e, a)
          | (e, INR b) => bP (e, b)
      end
      val unit = mk (Thunk.mk "()")
      fun C0 c = const (txt (Generics.Con.toString c))
      fun C1 c =
          case txt (Generics.Con.toString c)
           of c =>
              fn aT => case getT aT
                        of aP => fn ? => nest 1 (group (c <$> atomize (aP ?)))
      val data = getS

      val Y = Tie.function

      fun exn ? = !exnHandler ?
      fun regExn0 c = case C0 c of uP => regExn uP o Pair.snd
      fun regExn1 c aT = case C1 c aT of aP => regExn aP o Pair.snd

      fun refc aT = cyclic (Arg.refc ignore aT) o flip inj ! |< C1 ctorRef aT
      fun array aT = cyclic (Arg.array ignore aT) |<
                     sequ hashParens Array.toList (getT aT)

      fun vector aT = sequ hashBrackets Vector.toList (getT aT)
      fun list aT = sequ brackets id (getT aT)

      fun op --> _ = const txtFn

      local
         open Prettier
         val toLit = txt o String.toString
         val txtNlBs = txt "\\n\\"
      in
         fun string (_, s) =
             (true,
              group o dquotes |< choice
                 {wide = toLit s,
                  narrow = lazy (fn () =>
                                    List.foldl1
                                       (fn (x, s) =>
                                           s <^> txtNlBs <$> backslash <^> x)
                                       (List.map toLit
                                                 (String.fields
                                                     (#"\n" <\ op =) s)))})
      end

      val bool = mk Bool.toString
      val char = mk (enc "#\"" "\"" Char.toString)
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
