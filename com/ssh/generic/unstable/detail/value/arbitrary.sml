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

   fun variantHash aT = G.variant o Word32.toWord o Arg.hash (aT ())

   fun mkInt (Ops.I {precision, isoLarge = (_, fromLarge), ...}) aT = let
      fun gen n =
          map (fn i => fromLarge (i - IntInf.<< (1, Word.fromInt n - 0w1)))
              (G.bits n)
   in
      IN {gen = case precision
                 of NONE   => G.sized (0 <\ G.intInRange) >>= gen o 1 <\ op +
                  | SOME n => G.intInRange (1, n) >>= gen,
          cog = variantHash aT}
   end

   fun mkReal fromReal aT =
       IN {gen = G.sized ((fn r => map fromReal (G.realInRange (~r,r))) o real),
           cog = variantHash aT}

   fun mkWord (Ops.W {wordSize, isoLargeInt = (_, fromLargeInt), ...}) aT =
       IN {gen = map fromLargeInt (G.bits wordSize),
           cog = variantHash aT}

   fun iso' (IN {gen, cog}) (a2b, b2a) =
       IN {gen = map b2a gen, cog = cog o a2b}

   val exns : Exn.t G.t Buffer.t = Buffer.new ()

   fun list' (IN {gen = xGen, cog = xCog}) = let
      val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
      fun xsCog [] = G.variant 0w0
        | xsCog (x::xs) =
          universally (xCog x) o G.variant 0w1 o universally (xsCog xs)
   in
      IN {gen = xsGen, cog = xsCog}
   end

   structure ArbitraryRep = LayerRep' (open Arg type 'a t = 'a t)

   open ArbitraryRep.This

   fun arbitrary ? = #gen (out (getT ?))
   fun withGen gen = mapT (fn IN {cog, ...} => IN {gen = gen, cog = cog})

   structure Open = LayerDepCases
     (fun iso        aT = iso' (getT aT)
      fun isoProduct aP = iso' (getP aP)
      fun isoSum     aS = iso' (getS aS)

      fun op *` (aP, bP) = let
         val IN {gen = aG, cog = aC} = getP aP
         val IN {gen = bG, cog = bC} = getP bP
      in
         IN {gen = G.Monad.>< (aG, bG), cog = fn a & b => aC a o bC b}
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val IN {gen = aG, cog = aC} = getS aS
         val IN {gen = bG, cog = bC} = getS bS
         val aG = map INL aG
         val bG = map INR bG
         val gen = G.frequency [(Arg.numAlts aS, aG),
                                (Arg.numAlts bS, bG)]
         val gen0 =
             case Arg.hasBaseCase aS & Arg.hasBaseCase bS
              of true  & false => aG
               | false & true  => bG
               | _             => gen
      in
         IN {gen = G.sized (fn 0 => gen0 | _ => gen),
             cog = fn INL a => G.variant 0w0 o aC a
                    | INR b => G.variant 0w1 o bC b}
      end
      val unit = IN {gen = G.return (), cog = const (G.variant 0w0)}
      fun C0 _ = unit
      fun C1 _ = getT
      val data = getS

      fun Y ? = let open Tie in iso (G.Y *` function) end
                   (fn IN {gen = a, cog = b} => a & b,
                    fn a & b => IN {gen = a, cog = b}) ?

      fun aT --> bT = let
         val IN {gen = aG, cog = aC} = getT aT
         val IN {gen = bG, cog = bC} = getT bT
      in
         IN {gen = G.promote (fn a => universally (aC a) bG),
             cog = fn f => fn g => aG >>= (fn a => universally (bC (f a)) g)}
      end

      val exn = IN {gen = G.return () >>= (fn () =>
                          G.intInRange (0, Buffer.length exns-1) >>= (fn i =>
                          Buffer.sub (exns, i))),
                    cog = variantHash Arg.Open.exn}
      fun regExn0 _ (e, _) = Buffer.push exns (G.return e)
      fun regExn1 _ aT (a2e, _) = Buffer.push exns (map a2e (arbitrary aT))

      fun list ? = list' (getT ?)
      fun vector a = iso' (list a) Vector.isoList

      fun array  a = iso' (list a) Array.isoList

      fun refc a = iso' (getT a) (!, ref)

      val fixedInt = mkInt FixedIntOps.ops Arg.Open.fixedInt
      val largeInt = mkInt LargeIntOps.ops Arg.Open.largeInt

      val largeWord = mkWord LargeWordOps.ops Arg.Open.largeWord
      val largeReal = mkReal R.toLarge Arg.Open.largeReal

      val bool = IN {gen = G.bool, cog = G.variant o W.fromInt o Bool.toInt}
      val char = IN {gen = map Byte.byteToChar G.word8,
                     cog = G.variant o Word8.toWord o Byte.charToByte}
      val int = mkInt IntOps.ops Arg.Open.int
      val real = mkReal id Arg.Open.real
      val string = iso' (list' char) String.isoList
      val word = IN {gen = G.lift G.RNG.value, cog = G.variant}

      val word8 = IN {gen = G.word8, cog = G.variant o Word8.toWord}
      val word32 = mkWord Word32Ops.ops Arg.Open.word32
(*
      val word64 = mkWord Word64Ops.ops Arg.Open.word64
*)

      fun hole () = IN {gen = G.lift undefined, cog = undefined}

      open Arg ArbitraryRep)
end
