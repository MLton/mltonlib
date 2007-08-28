(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX show sharing *)
(* XXX pretty printing could use some tuning *)
(* XXX parameters for pretty printing? *)
(* XXX parameters for depth, length, etc... for showing only partial data *)

functor WithPretty (Arg : OPEN_CASES) : PRETTY_CASES = struct
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
      fun atomic    doc = (true,  doc)
      fun nonAtomic doc = (false, doc)
      val uop : t UnOp.t -> u UnOp.t = id <\ Pair.map
      val bop : t BinOp.t -> u BinOp.t =
          fn f => nonAtomic o f o Pair.map (Sq.mk Pair.snd)
   in
      type u = u

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

   local
      open Generics
   in
      val C = C
      val l2s = Label.toString
      val c2s = Con.toString
   end

   fun inj b a2b = b o Pair.map (id, a2b)

   val txtAs = txt "as"
   val txtFn = txt "#fn"

   val ctorRef = C "ref"

   fun cyclic t =
       case Univ.Emb.new ()
        of (to, from) =>
           fn (e, v : ''a) => let
                 val idx = Int.toString o length
                 fun lp [] = let
                        val c = ref true
                        val r = t (to (v, c)::e, v)
                     in
                        if !c then r else txt ("#"^idx e) </> txtAs </> r
                     end
                   | lp (u::e) =
                     case from u
                      of NONE => lp e
                       | SOME (x, c) =>
                         if x <> v then lp e else (c := false ; txt ("#"^idx e))
              in
                 lp e
              end

   fun sequ style toL t (e, a) =
       surround style o fill o punctuate comma o List.map (curry t e) |< toL a

   type 'a t = Univ.t List.t * 'a -> u

   fun mk toS : 'a t = txt o toS o Pair.snd
   fun enc l r toS x = concat [l, toS x, r]
   fun mkWord toString = mk ("0wx" <\ op ^ o toString)

   val exnHandler : Exn.t t Ref.t =
       ref (txt o "#" <\ op ^ o General.exnName o #2)

   structure Pretty = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Pretty.This

   fun layout t = Pair.snd o [] <\ getT t
   fun pretty m t = Prettier.pretty m o layout t
   fun show t = pretty NONE t

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Pretty and Rep = Pretty.Closed

      fun iso b = inj b o Iso.to
      val isoProduct = iso
      val isoSum = iso

      fun (l *` r) (e, a & b) = l (e, a) <^> comma <$> r (e, b)
      val T = id
      fun R l = case txt (l2s l)
                 of l => fn t => fn ? => group (nest 1 (l </> equals </> t ?))
      fun tuple t = surround parens o t
      fun record t = surround braces o t

      fun l +` r = fn (e, INL a) => l (e, a)
                    | (e, INR b) => r (e, b)
      val unit = mk (Thunk.mk "()")
      fun C0 c = const (txt (c2s c))
      fun C1 c = case txt (c2s c)
                  of c => fn t => fn ? => nest 1 (group (c <$> atomize (t ?)))
      val data = id

      val Y = Tie.function

      fun exn ? = !exnHandler ?
      fun regExn0 c (_, prj) =
          Ref.modify (fn exnHandler => fn (env, e) =>
                         case prj e
                          of NONE    => exnHandler (env, e)
                           | SOME () => txt (c2s c)) exnHandler
      fun regExn1 c t (_, prj) =
          Ref.modify (fn exnHandler => fn (env, e) =>
                         case prj e
                          of NONE   => exnHandler (env, e)
                           | SOME x =>
                             nest 1 (group (txt (c2s c) <$>
                                            atomize (t (env, x))))) exnHandler

      fun refc ? = cyclic o flip inj ! |< C1 ctorRef ?
      fun array ? = cyclic |< sequ hashParens Array.toList ?

      fun vector ? = sequ hashBrackets Vector.toList ?
      fun list ? = sequ brackets id ?

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
