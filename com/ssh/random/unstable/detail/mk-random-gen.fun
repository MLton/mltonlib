(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRandomGen (RNG : RNG) :>
   RANDOM_GEN where type RNG.t = RNG.t
              where type RNG.Seed.t = RNG.Seed.t = struct
   (* <-- SML/NJ workarounds *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   infix  1 >>=
   (* SML/NJ workarounds --> *)

   structure A = Array and R = Real and V = Vector and W = Word

   fun assert th = if th () then () else fail "assertion failed"

   structure RNG = RNG

   type 'a t = Int.t * RNG.t -> 'a

   fun generate n t =
       pass (W.toInt (RNG.value t mod W.fromInt n), RNG.next t)

   fun lift r2a = r2a o Pair.snd

   structure Monad =
      MkMonad (type 'a monad = 'a t
               val return = const
               fun (m >>= k) (n, r) =
                   k (m (n, RNG.split 0wx4969599B r))
                     (n, RNG.split 0wx1AB25A6D r))

   open Monad

   fun map a2b ga = a2b o ga

   fun promote a2b (n, r) a = a2b a (n, r)

   fun variant v m = m o Pair.map (id, RNG.split v)

   fun mapUnOp (to, from) eG2eG = let
      fun map f g = f o g
   in
      Fn.map (map to, map from) eG2eG
   end

   fun sized i2g (n, r) = i2g n (n, r)
   fun resize f g = g o Pair.map (f, id)
   fun bool (_, r) = RNG.maxValue div 0w2 < RNG.value r
   local
      val n = 0w256 val d = RNG.maxValue div n val m = d * n
   in
      fun word8 (_, r) = Word8.fromWord (RNG.value r mod m div d)
   end

   fun Y ? = Tie.pure (fn () => let
                             val r = ref (raising Fix.Fix)
                             fun f x = !r x
                          in
                             (resize (op div /> 2) f,
                              fn f' => (r := f' ; f'))
                          end) ?

   fun inRange bInRange (a2b, b2a) =
       map b2a o bInRange o Pair.map (Sq.mk a2b)

   fun list aG n =
       if n < 0 then raise Domain
       else fn (s, r) =>
               List.unfoldl (fn 0w0 => NONE
                              | i   => SOME (aG (s, RNG.split i r), i-0w1))
                            (W.fromInt n)

   fun bits n = (* XXX this is O(n*n), O(n) is possible via IntInf.scan *)
       if n < 0 then raise Domain else let
          val msk = IntInf.<< (1, Word.fromInt n) - 1
       in
          lift (fn r => let
                      fun lp (n, r, i) =
                          if 0 < n
                          then lp (n - 8,
                                   RNG.next r,
                                   IntInf.<< (i, 0w8) +
                                   Word8.toLargeInt (word8 (0, r)))
                          else IntInf.andb (i, msk)
                   in
                      lp (n, r, 0)
                   end)
       end

   fun wordInRange (l, h) =
       (assert (fn () => l <= h)
      ; let val n = h - l + 0w1         (* XXX may overflow *)
            val d = RNG.maxValue div n  (* XXX may result in zero *)
            val m = d * n
        in lift (fn r => RNG.value r mod m div d + l)
        end)

   fun intInRange (l, h) =
       (assert (fn () => l <= h)
      ; map (op + /> l)
            (inRange wordInRange (Iso.swap W.isoInt) (0, h - l)))

   local
      val () = if R.radix <> 2 then fail "Real.radix <> 2" else ()
      val d = R.fromLargeInt (IntInf.<< (1, W.fromInt R.precision) - 1)
          handle _ => Math.pow (2.0, real R.precision) - 1.0
          (* XXX The handler is/was a MLKit workaround; see Extended Basis *)
   in
      fun realInRange (l, h) =
          (assert (fn () => l <= h)
         ; let val m = (h - l) / d
           in map (fn i => R.fromLargeInt i * m + l) (bits R.precision)
           end)
   end

   fun elements xs = let
      val xs = V.fromList xs
   in
      map (xs <\ V.sub) (intInRange (0, V.length xs))
   end

   fun oneOf gs = elements gs >>= id

   fun frequency xs = let
      val xs = A.fromList xs
      val tot = A.foldli (fn (i, (n, g), tot) =>
                             (A.update (xs, i, (n+tot, g)) ; n+tot))
                         0 xs
      fun pick i n = let
         val (k, x) = A.sub (xs, i)
      in
         if n <= k then x else pick (i+1) n
      end
   in
      intInRange (1, tot) >>= pick 0
   end
end
