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

   structure RandomGen = Arg.RandomGen

   structure G = RandomGen and I = Int and R = Real and W = Word

   datatype 'a u = IN of {gen : 'a G.t, cog : 'a -> Univ.t G.t UnOp.t}
   fun out (IN r) = r

   structure Rep : OPEN_GENERIC_REP = struct
      fun get get = Pair.snd o get
      fun map map f = map (Pair.map (id, f))

      type ('a, 'x) t = ('a, 'a u * 'x) Arg.Rep.t
      fun getT ? = get Arg.Rep.getT ?
      fun mapT ? = map Arg.Rep.mapT ?

      type ('a, 'x) s = ('a, 'a u * 'x) Arg.Rep.s
      fun getS ? = get Arg.Rep.getS ?
      fun mapS ? = map Arg.Rep.mapS ?

      type ('a, 'k, 'x) p = ('a, 'k, 'a u * 'x) Arg.Rep.p
      fun getP ? = get Arg.Rep.getP ?
      fun mapP ? = map Arg.Rep.mapP ?
   end

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

   fun morph outer f = outer (fn (a, x) => fn i => (iso' a i, f x i))
   fun nullary outer t x = outer (t, x)
   fun binop outer f g = outer (Pair.map (f, g) o Pair.swizzle)
   fun unary outer f g = outer (Pair.map (f, g))

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?

   val unit' = IN {gen = G.return (), cog = const (G.variant 0)}
   fun unit ? = nullary Arg.unit unit' ?
   fun bool ? =
       nullary Arg.bool (IN {gen = G.bool, cog = G.variant o Bool.toInt}) ?

   val int' = IN {gen = map (fn w => W.toIntX (w - G.RNG.maxValue div 0w2))
                            (* XXX result may not fit an Int.t *)
                            (G.lift G.RNG.value),
                  cog = G.variant}
   fun int ? = nullary Arg.int int' ?

   val word' = IN {gen = G.lift G.RNG.value, cog = G.variant o W.toIntX}
   fun word ? = nullary Arg.word word' ?

   fun Y y = Arg.Y (let open Tie in iso (G.Y *` function *` y) end
                       (fn (IN {gen = a, cog = b}, c) => a & b & c,
                        fn a & b & c => (IN {gen = a, cog = b}, c)))

   fun op *` ? = binop Arg.*`
                       (fn (IN {gen = aGen, cog = aCog},
                            IN {gen = bGen, cog = bCog}) =>
                           IN {gen = G.Monad.>>& (aGen, bGen),
                               cog = fn a & b => aCog a o bCog b}) ?

   fun op +` xy2z (a, b) =
       binop Arg.+`
             (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =>
                 let
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
       binop Arg.-->
             (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =>
                 IN {gen = G.promote (fn a => universally (aCog a) bGen),
                     cog = fn f => fn g =>
                              aGen >>= (fn a => universally (bCog (f a)) g)}) ?

   fun exn ? = let
      val e = Fail "Arbitrary.exn not supported yet"
   in
      nullary Arg.exn (IN {gen = G.return Empty, cog = raising e})
   end ?

   fun regExn ef = Arg.regExn (ef o Pair.snd)

   fun list' (IN {gen = xGen, cog = xCog}) = let
      val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
      fun xsCog [] = G.variant 0
        | xsCog (x::xs) =
          universally (xCog x) o G.variant 1 o universally (xsCog xs)
   in
      IN {gen = xsGen, cog = xsCog}
   end
   fun list ? = unary Arg.list list' ?
   val char' = IN {gen = map chr (G.intInRange (0, Char.maxOrd)),
                   cog = G.variant o ord}
   fun char ? = nullary Arg.char char' ?
   val string' as IN {cog = stringCog', ...} = iso' (list' char') String.isoList
   fun string ? = nullary Arg.string string' ?

   fun array ? = unary Arg.array (fn a => iso' (list' a) Array.isoList) ?
   fun refc ? = unary Arg.refc (fn a => iso' a (!, ref)) ?
   fun vector ? = unary Arg.vector (fn a => iso' (list' a) Vector.isoList) ?

   fun largeInt  ? = nullary Arg.largeInt  (iso' int'  (Iso.swap I.isoLarge)) ?
   fun largeWord ? = nullary Arg.largeWord (iso' word' (Iso.swap W.isoLarge)) ?

   val real' = IN {gen = G.sized ((fn r => G.realInRange (~r, r)) o real),
                   cog = stringCog' o R.toString} (* XXX Real cog *)

   fun real ? = nullary Arg.real real' ?
   fun largeReal ? =
       nullary Arg.largeReal
               (iso' real' (Iso.swap (R.isoLarge IEEEReal.TO_NEAREST))) ?

   local
      fun mk outer large =
          nullary outer (iso' word' (Iso.<--> (Iso.swap W.isoLarge, large)))
   in
      fun word8  ? = mk Arg.word8  Word8.isoLarge  ?
   (* fun word16 ? = mk Arg.word16 Word16.isoLarge ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = mk Arg.word32 Word32.isoLarge ?
      fun word64 ? = mk Arg.word64 Word64.isoLarge ?
   end

   (* Trivialities *)

   fun T ? = unary Arg.T id ?
   fun R f = Arg.R (fn l => Pair.map (id, f l))
   fun tuple ? = unary Arg.tuple id ?
   fun record ? = unary Arg.record id ?
   fun C0 f = Arg.C0 (fn l => (unit', f l))
   fun C1 f = Arg.C1 (fn l => Pair.map (id, f l))
   fun data ? = unary Arg.data id ?
end
