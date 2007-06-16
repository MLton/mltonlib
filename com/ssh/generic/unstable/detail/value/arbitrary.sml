(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_GENERIC = struct
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
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   open GenericsUtil

   structure RandomGen = Arg.RandomGen

   structure G = RandomGen and I = Int and R = Real and W = Word

   datatype 'a t = IN of {gen : 'a G.t, cog : 'a -> Univ.t G.t UnOp.t}
   fun out (IN r) = r

   structure Rep =
      JoinGenericReps
         (structure Outer = Arg.Rep
          structure Inner =
             OpenGenericRep (MkClosedGenericRep (type 'a t = 'a t)))

   structure Arbitrary = Rep

   fun universally ? = G.mapUnOp (Univ.newIso ()) ?

   val map = G.Monad.map
   val op >>= = G.>>=

   fun arbitrary ? = (#gen o out o Pair.fst o Arg.Rep.getT) ? 
   fun withGen gen =
       Arg.Rep.mapT
          (Pair.map (fn IN {cog, ...} => IN {gen = gen,cog = cog},
                     id))

   fun iso' (IN {gen, cog}) (a2b, b2a) =
       IN {gen = map b2a gen, cog = cog o a2b}

   fun iso ? = morph Arg.iso iso' ?
   fun isoProduct ? = morph Arg.isoProduct iso' ?
   fun isoSum ? = morph Arg.isoSum iso' ?

   val unit' = IN {gen = G.return (), cog = const (G.variant 0)}
   fun unit ? = op0 Arg.unit unit' ?
   fun bool ? = op0 Arg.bool (IN {gen = G.bool, cog = G.variant o Bool.toInt}) ?

   val int' = IN {gen = map (fn w => W.toIntX (w - G.RNG.maxValue div 0w2))
                            (* XXX result may not fit an Int.t *)
                            (G.lift G.RNG.value),
                  cog = G.variant}
   fun int ? = op0 Arg.int int' ?

   val word' = IN {gen = G.lift G.RNG.value, cog = G.variant o W.toIntX}
   fun word ? = op0 Arg.word word' ?

   fun Y ? = y Arg.Y (let open Tie in iso (G.Y *` function) end
                         (fn IN {gen = a, cog = b} => a & b,
                          fn a & b => IN {gen = a, cog = b})) ?

   fun op *` ? = op2 Arg.*`
                     (fn (IN {gen = aGen, cog = aCog},
                          IN {gen = bGen, cog = bCog}) =>
                         IN {gen = G.Monad.>>& (aGen, bGen),
                             cog = fn a & b => aCog a o bCog b}) ?

   fun op +` xy2z (a, b) =
       op2 Arg.+`
           (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) => let
               val aGen = map INL aGen
               val bGen = map INR bGen
               val gen = G.frequency [(Arg.numAlts a, aGen),
                                      (Arg.numAlts b, bGen)]
               val gen0 =
                   case Arg.hasBaseCase a & Arg.hasBaseCase b of
                      true & false => aGen
                    | false & true => bGen
                    | _            => gen
            in
               IN {gen = G.sized (fn 0 => gen0 | _ => gen),
                   cog = fn INL a => G.variant 0 o aCog a
                          | INR b => G.variant 1 o bCog b}
            end) xy2z (a, b)

   fun op --> ? =
       op2 Arg.-->
           (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =>
               IN {gen = G.promote (fn a => universally (aCog a) bGen),
                   cog = fn f => fn g =>
                            aGen >>= (fn a => universally (bCog (f a)) g)}) ?

   fun exn ? =
       op0 Arg.exn (IN {gen = G.return Empty,
                        cog = failing "Arbitrary.exn unsupported"}) ?

   fun regExn ? = re Arg.regExn (const ignore) ?

   fun list' (IN {gen = xGen, cog = xCog}) = let
      val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
      fun xsCog [] = G.variant 0
        | xsCog (x::xs) =
          universally (xCog x) o G.variant 1 o universally (xsCog xs)
   in
      IN {gen = xsGen, cog = xsCog}
   end
   fun list ? = op1 Arg.list list' ?
   val char' = IN {gen = map chr (G.intInRange (0, Char.maxOrd)),
                   cog = G.variant o ord}
   fun char ? = op0 Arg.char char' ?
   val string' as IN {cog = stringCog', ...} = iso' (list' char') String.isoList
   fun string ? = op0 Arg.string string' ?

   fun array ? = op1 Arg.array (fn a => iso' (list' a) Array.isoList) ?
   fun refc ? = op1 Arg.refc (fn a => iso' a (!, ref)) ?
   fun vector ? = op1 Arg.vector (fn a => iso' (list' a) Vector.isoList) ?

   fun largeInt  ? = op0 Arg.largeInt  (iso' int'  (Iso.swap I.isoLarge)) ?
   fun largeWord ? = op0 Arg.largeWord (iso' word' (Iso.swap W.isoLarge)) ?

   val real' = IN {gen = G.sized ((fn r => G.realInRange (~r, r)) o real),
                   cog = stringCog' o R.toString} (* XXX Real cog *)

   fun real ? = op0 Arg.real real' ?
   fun largeReal ? =
       op0 Arg.largeReal
           (iso' real' (Iso.swap (R.isoLarge IEEEReal.TO_NEAREST))) ?

   local
      fun mk outer large =
          op0 outer (iso' word' (Iso.<--> (Iso.swap W.isoLarge, large)))
   in
      fun word8  ? = mk Arg.word8  Word8.isoLarge  ?
   (* fun word16 ? = mk Arg.word16 Word16.isoLarge ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = mk Arg.word32 Word32.isoLarge ?
      fun word64 ? = mk Arg.word64 Word64.isoLarge ?
   end

   (* Trivialities *)

   fun T ? = t Arg.T id ?
   fun R ? = r Arg.R (const id) ?
   fun tuple ? = op1 Arg.tuple id ?
   fun record ? = op1 Arg.record id ?
   fun C0 ? = c0 Arg.C0 (const unit') ?
   fun C1 ? = c1 Arg.C1 (const id) ?
   fun data ? = op1 Arg.data id ?
end
