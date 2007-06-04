(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRandomGen (RNG : RNG) :>
   RANDOM_GEN where type RNG.t = RNG.t
              where type RNG.Seed.t = RNG.Seed.t =
struct
   structure A = Array and R = Real and V = Vector and W = Word

   fun assert th = if th () then () else fail "assertion failed"

   structure RNG = RNG

   type 'a dom = Int.t * RNG.t and 'a cod = 'a
   type 'a t = 'a dom -> 'a cod

   fun generate n t =
       pass (W.toInt (RNG.value t mod (W.fromInt n)), RNG.next t)

   fun lift r2a = r2a o Pair.snd

   structure Monad =
      MkMonad (type 'a monad = 'a t
               val return = const
               fun (m >>= k) (n, r) =
                   k (m (n, RNG.split 0w314 r)) (n, RNG.split 0w159 r))

   open Monad

   fun map a2b ga = a2b o ga

   fun promote a2b (n, r) a = a2b a (n, r)

   fun variant v m = m o Pair.map (id, RNG.split (W.fromInt v + 0w1))

   fun mapUnOp (to, from) eG2eG = let
      fun map f g = f o g
   in
      Fn.map (map to, map from) eG2eG
   end

   fun sized i2g (n, r) = i2g n (n, r)
   fun resize f g = g o Pair.map (f, id)
   fun bool (_, r) = RNG.maxValue div 0w2 < RNG.value r

   fun Y ? = Tie.pure (fn () => let
                             val r = ref (raising Fix.Fix)
                             fun f x = !r x
                          in
                             (resize (op div /> 2) f,
                              fn f' => (r := f' ; f'))
                          end) ?

   fun inRange bInRange (a2b, b2a) =
       map b2a o bInRange o Pair.map (Sq.mk a2b)

   fun wordInRange (l, h) =
       (assert (fn () => l <= h)
      ; let val n = h - l + 0w1         (* XXX may overflow *)
            val d = RNG.maxValue div n  (* XXX may result in zero *)
            val m = n * d
        in lift (fn r => RNG.value r mod m div d + l)
        end)

   fun intInRange (l, h) =
       (assert (fn () => l <= h)
      ; map (op + /> l)
            (inRange wordInRange (Iso.swap W.isoInt) (0, h - l)))

   local
      val w2r = R.fromLargeInt o W.toLargeInt
   in
      fun realInRange (l, h) =
          (assert (fn () => l <= h)
         ; let val m = (h - l) / w2r RNG.maxValue
           in fn (_, r) => w2r (RNG.value r) * m + l
           end)
   end

   fun elements xs =
       let val xs = V.fromList xs
       in map (xs <\ V.sub) (intInRange (0, V.length xs))
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

   local
      fun unfold px sx x2y = let
         fun lp ys x =
             if px x then
                rev ys
             else
                lp (x2y x::ys) (sx x)
      in
         lp []
      end
   in
      fun list ga m (n, r) =
          unfold (op = /> 0w0)
                 (op - /> 0w1)
                 (fn i => ga (n, RNG.split i r))
                 (W.fromInt m)
   end
end
