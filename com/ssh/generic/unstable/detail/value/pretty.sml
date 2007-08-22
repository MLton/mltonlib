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

   structure Pretty = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = exn list * 'a -> u))

   open Pretty.This

   fun layout t = Pair.snd o [] <\ getT t
   fun pretty m t = Prettier.pretty m o layout t

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Pretty and Rep = Pretty.Closed

      local
         open Generics
      in
         val C = C
         val l2s = Label.toString
         val c2s = Con.toString
      end

      fun inj b a2b = b o Pair.map (id, a2b)
      fun iso b = inj b o Iso.to
      val isoProduct = iso
      val isoSum = iso

      fun (l *` r) (env, a & b) = l (env, a) <^> comma <$> r (env, b)

      val T = id
      fun R label = let
         val txtLabel = txt (l2s label)
         fun fmt t ? = group (nest 1 (txtLabel </> equals </> t ?))
      in
         fmt
      end

      fun tuple t = surround parens o t
      fun record t = surround braces o t

      fun l +` r = fn (env, INL a) => l (env, a)
                    | (env, INR b) => r (env, b)

      fun C0 ctor = const (txt (c2s ctor))
      fun C1 ctor = let
         val txtCtor = txt (c2s ctor)
      in
         fn t => fn ? => nest 1 (group (txtCtor <$> atomize (t ?)))
      end

      val data = id

      val Y = Tie.function

      val exn : Exn.t Rep.t ref =
          ref (txt o "#" <\ op ^ o General.exnName o #2)
      fun regExn0 c (_, prj) =
          Ref.modify (fn exn => fn (env, e) =>
                         case prj e
                          of NONE    => exn (env, e)
                           | SOME () => txt (c2s c)) exn
      fun regExn1 c t (_, prj) =
          Ref.modify (fn exn => fn (env, e) =>
                         case prj e
                          of NONE   => exn (env, e)
                           | SOME x =>
                             nest 1 (group (txt (c2s c) <$>
                                            atomize (t (env, x))))) exn


      val exn = fn ? => !exn ?

      val txtAs = txt "as"
      fun cyclic t = let
         exception E of ''a * bool ref
      in
         fn (env, v : ''a) => let
               val idx = Int.toString o length
               fun lp (E (v', c)::env) =
                   if v' <> v then
                      lp env
                   else
                      (c := false ; txt ("#"^idx env))
                 | lp (_::env) = lp env
                 | lp [] = let
                      val c = ref true
                      val r = t (E (v, c)::env, v)
                   in
                      if !c then
                         r
                      else
                         txt ("#"^idx env) </> txtAs </> r
                   end
            in
               lp env
            end
      end
      fun aggregate style toL t (env, a) =
          surround style o fill o punctuate comma o List.map (curry t env) |< toL a

      val ctorRef = C "ref"
      fun refc  ? = cyclic o flip inj ! |< C1 ctorRef ?
      fun array ? = cyclic |< aggregate hashParens Array.toList ?

      fun vector ? = aggregate hashBrackets Vector.toList ?

      fun list ? = aggregate brackets id ?

      val txtFn = txt "#fn"
      fun _ --> _ = const txtFn

      local
         open Prettier
         val toLit = txt o String.toString
         val nlbs = txt "\\n\\"
      in
      fun string (_, s) =
          (true,
           group o dquotes |< choice
              {wide = toLit s,
               narrow = lazy (fn () =>
                                 List.foldl1
                                    (fn (x, s) =>
                                        s <^> nlbs <$> backslash <^> x)
                                    (List.map toLit
                                              (String.fields
                                                  (#"\n" <\ op =) s)))})
      end

      fun mk toS : 'a Rep.t = txt o toS o Pair.snd
      fun enc l r toS x = concat [l, toS x, r]
      fun mkWord toString = mk ("0wx" <\ op ^ o toString)

      val bool = mk Bool.toString
      val char = mk (enc "#\"" "\"" Char.toString)
      val int  = mk Int.toString
      val real = mk Real.toString
      val unit = mk (Thunk.mk "()")
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
