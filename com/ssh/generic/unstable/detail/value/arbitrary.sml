(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithArbitrary (Arg : WITH_ARBITRARY_DOM) : ARBITRARY_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse >>=
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure RandomGen = Arg.RandomGen

   structure G = RandomGen and R = Real and W = Word

   fun universally ? = G.mapUnOp (Univ.Iso.new ()) ?
   val map = G.Monad.map
   val op >>= = G.>>=

   datatype 'a t = IN of {gen : 'a G.t, cog : 'a -> Univ.t G.t UnOp.t}
   fun out (IN r) = r

   structure Arbitrary = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Arbitrary.This

   fun cogS ? = #cog (out (getS ?))
   fun genS ? = #gen (out (getS ?))

   fun arbitrary ? = #gen (out (getT ?))
   fun withGen gen = mapT (fn IN {cog, ...} => IN {gen = gen, cog = cog})

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Arbitrary

      fun iso' (IN {gen, cog}) (a2b, b2a) =
          IN {gen = map b2a gen, cog = cog o a2b}

      fun iso        ? = iso' (getT ?)
      fun isoProduct ? = iso' (getP ?)
      fun isoSum     ? = iso' (getS ?)

      fun op *`` (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =
          IN {gen = G.Monad.>>& (aGen, bGen), cog = fn a & b => aCog a o bCog b}
      fun op *` (a, b) = op *`` (getP a, getP b)
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val aGen = map INL (genS aS)
         val bGen = map INR (genS bS)
         val gen = G.frequency [(Arg.numAlts aS, aGen),
                                (Arg.numAlts bS, bGen)]
         val gen0 =
             case Arg.hasBaseCase aS & Arg.hasBaseCase bS of
                true & false => aGen
              | false & true => bGen
              | _            => gen
      in
         IN {gen = G.sized (fn 0 => gen0 | _ => gen),
             cog = fn INL a => G.variant 0w0 o cogS aS a
                    | INR b => G.variant 0w1 o cogS bS b}
      end
      val unit = IN {gen = G.return (), cog = const (G.variant 0w0)}
      fun C0 _ = unit
      fun C1 _ = getT
      val data = getS

      fun Y ? = let open Tie in iso (G.Y *` function) end
                   (fn IN {gen = a, cog = b} => a & b,
                    fn a & b => IN {gen = a, cog = b}) ?

      fun op -->` (IN {gen = aGen, cog = aCog}, IN {gen = bGen, cog = bCog}) =
          IN {gen = G.promote (fn a => universally (aCog a) bGen),
              cog = fn f => fn g =>
                       aGen >>= (fn a => universally (bCog (f a)) g)}
      fun op --> (a, b) = op -->` (getT a, getT b)

      val exn = IN {gen = G.return Empty,
                    cog = failing "Arbitrary.exn unsupported"}
      fun regExn _ _ = ()

      fun list' (IN {gen = xGen, cog = xCog}) = let
         val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
         fun xsCog [] = G.variant 0w0
           | xsCog (x::xs) =
             universally (xCog x) o G.variant 0w1 o universally (xsCog xs)
      in
         IN {gen = xsGen, cog = xsCog}
      end
      fun list ? = list' (getT ?)

      fun array  a = iso' (list a) Array.isoList
      fun vector a = iso' (list a) Vector.isoList

      fun refc a = iso' (getT a) (!, ref)

      val char = IN {gen = map chr (G.intInRange (0, Char.maxOrd)),
                     cog = G.variant o W.fromInt o ord}
      val string as IN {cog = stringCog, ...} = iso' (list' char) String.isoList

      val bool = IN {gen = G.bool, cog = G.variant o W.fromInt o Bool.toInt}

      val fixedInt =
          IN {gen = map (fn w => W.toFixedIntX (w - G.RNG.maxValue div 0w2))
                        (G.lift G.RNG.value),
              cog = G.variant o W.fromFixedInt}
      val word = IN {gen = G.lift G.RNG.value, cog = G.variant}
      val real = IN {gen = G.sized ((fn r => G.realInRange (~r, r)) o real),
                     cog = stringCog o R.toString} (* XXX Real cog *)

      val      int = iso' fixedInt      Int.isoFixedInt
      val largeInt = iso' fixedInt LargeInt.isoFixedInt

      val largeWord = iso' word (Iso.swap W.isoLarge)
      val largeReal = iso' real (Iso.swap (R.isoLarge IEEEReal.TO_NEAREST))

      local
         fun mk large = iso' word (Iso.<--> (Iso.swap W.isoLarge, large))
      in
         val word8  = mk Word8.isoLarge
         val word32 = mk Word32.isoLarge
         val word64 = mk Word64.isoLarge
      end)

   open Layered
end
