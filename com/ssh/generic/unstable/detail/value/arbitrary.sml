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

   open Arg

   structure G = RandomGen and I = Int and R = Real and W = Word
         and Typ = TypeInfo

   datatype 'a u = IN of {gen : 'a G.t, cog : 'a -> Univ.t G.t UnOp.t}
   fun out (IN r) = r

   structure Index : EXT_GENERIC_INDEX = struct
      fun get get = Pair.snd o get
      fun map map f = map (Pair.map (id, f))

      type ('a, 'x) t = ('a, 'a u * 'x) Outer.Index.t
      fun getT ? = get Outer.Index.getT ?
      fun mapT ? = map Outer.Index.mapT ?

      type ('a, 'x) s = ('a, 'a u * 'x) Outer.Index.s
      fun getS ? = get Outer.Index.getS ?
      fun mapS ? = map Outer.Index.mapS ?

      type ('a, 'k, 'x) p = ('a, 'k, 'a u * 'x) Outer.Index.p
      fun getP ? = get Outer.Index.getP ?
      fun mapP ? = map Outer.Index.mapP ?
   end

   structure Arbitrary = Index

   fun universally ? = G.mapUnOp (Univ.newIso ()) ?

   val map = G.Monad.map
   val op >>= = G.>>=

   fun arbitrary ? = (#gen o out o Pair.fst o Outer.Index.getT) ? 
   fun withGen gen =
       Outer.Index.mapT
          (Pair.map (fn IN {cog, ...} => IN {gen = gen,cog = cog},
                     id))

   fun iso' (IN {gen, cog}) (a2b, b2a) =
       IN {gen = map b2a gen, cog = cog o a2b}

   fun morph outer f = outer (fn (a, x) => fn i => (iso' a i, f x i))
   fun nullary outer t x = outer (t, x)
   fun binop outer f g = outer (Pair.map (f, g) o Pair.swizzle)
   fun unary outer f g = outer (Pair.map (f, g))

   fun iso ? = morph Outer.iso ?
   fun isoProduct ? = morph Outer.isoProduct ?
   fun isoSum ? = morph Outer.isoSum ?

   val unit' = IN {gen = G.return (), cog = const (G.variant 0)}
   fun unit ? = nullary Outer.unit unit' ?
   fun bool ? =
       nullary Outer.bool (IN {gen = G.bool, cog = G.variant o Bool.toInt}) ?

   val int' = IN {gen = map (fn w => W.toIntX (w - G.RNG.maxValue div 0w2))
                            (* XXX result may not fit an Int.int *)
                            (G.lift G.RNG.value),
                  cog = G.variant}
   fun int ? = nullary Outer.int int' ?

   val word' = IN {gen = G.lift G.RNG.value, cog = G.variant o W.toIntX}
   fun word ? = nullary Outer.word word' ?

   fun Y y = Outer.Y (let open Tie in iso (G.Y *` function *` y) end
                         (fn (IN {gen = a, cog = b}, c) => a & b & c,
                          fn a & b & c => (IN {gen = a, cog = b}, c)))

   fun op *` ? = binop Outer.*`
                       (fn (IN {gen = aGen, cog = aCog},
                            IN {gen = bGen, cog = bCog}) =>
                           IN {gen = G.Monad.>>& (aGen, bGen),
                               cog = fn a & b => aCog a o bCog b}) ?

   fun op +` xy2z (a, b) =
       binop Outer.+`
             (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =>
                 let
                    val aGen = map INL aGen
                    val bGen = map INR bGen
                    val gen = G.frequency [(Typ.numConsecutiveAlts a, aGen),
                                           (Typ.numConsecutiveAlts b, bGen)]
                    val gen0 =
                        case Typ.hasBaseCase a & Typ.hasBaseCase b of
                           true & false => aGen
                         | false & true => bGen
                         | _            => gen
                 in
                    IN {gen = G.sized (fn 0 => gen0 | _ => gen),
                        cog = fn INL a => G.variant 0 o aCog a
                               | INR b => G.variant 1 o bCog b}
                 end) xy2z (a, b)

   fun op --> ? =
       binop Outer.-->
             (fn (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =>
                 IN {gen = G.promote (fn a => universally (aCog a) bGen),
                     cog = fn f => fn g =>
                              aGen >>= (fn a => universally (bCog (f a)) g)}) ?

   fun exn ? = let
      val e = Fail "Arbitrary.exn not supported yet"
   in
      nullary Outer.exn (IN {gen = G.return Empty, cog = raising e})
   end ?

   fun regExn ef = Outer.regExn (ef o Pair.snd)

   fun list' (IN {gen = xGen, cog = xCog}) = let
      val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
      fun xsCog [] = G.variant 0
        | xsCog (x::xs) =
          universally (xCog x) o G.variant 1 o universally (xsCog xs)
   in
      IN {gen = xsGen, cog = xsCog}
   end
   fun list ? = unary Outer.list list' ?
   val char' = IN {gen = map chr (G.intInRange (0, Char.maxOrd)),
                   cog = G.variant o ord}
   fun char ? = nullary Outer.char char' ?
   val string' as IN {cog = stringCog', ...} = iso' (list' char') String.isoList
   fun string ? = nullary Outer.string string' ?

   fun array ? = unary Outer.array (fn a => iso' (list' a) Array.isoList) ?
   fun refc ? = unary Outer.refc (fn a => iso' a (!, ref)) ?
   fun vector ? = unary Outer.vector (fn a => iso' (list' a) Vector.isoList) ?

   fun largeInt  ? = nullary Outer.largeInt  (iso' int'  (Iso.swap I.isoLarge)) ?
   fun largeWord ? = nullary Outer.largeWord (iso' word' (Iso.swap W.isoLarge)) ?

   val real' = IN {gen = G.sized ((fn r => G.realInRange (~r, r)) o real),
                   cog = stringCog' o Real.toString} (* XXX Real cog *)

   fun real ? = nullary Outer.real real' ?
   fun largeReal ? =
       nullary Outer.largeReal
               (iso' real' (Iso.swap (R.isoLarge IEEEReal.TO_NEAREST))) ?

   local
      fun mk outer large =
          nullary outer (iso' word' (Iso.<--> (Iso.swap W.isoLarge, large)))
   in
      fun word8  ? = mk Outer.word8  Word8.isoLarge  ?
   (* fun word16 ? = mk Outer.word16 Word16.isoLarge ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = mk Outer.word32 Word32.isoLarge ?
      fun word64 ? = mk Outer.word64 Word64.isoLarge ?
   end

   (* Trivialities *)

   fun T ? = unary Outer.T id ?
   fun R f = Outer.R (fn l => Pair.map (id, f l))
   fun tuple ? = unary Outer.tuple id ?
   fun record ? = unary Outer.record id ?
   fun C0 f = Outer.C0 (fn l => (unit', f l))
   fun C1 f = Outer.C1 (fn l => Pair.map (id, f l))
   fun data ? = unary Outer.data id ?
end
