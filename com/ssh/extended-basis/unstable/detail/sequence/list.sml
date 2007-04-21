(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure List : LIST = struct
   open List
   val sub = nth
   fun init l = rev (tl (rev l))
   fun unfoldl' f = let
      fun lp ys x = case f x of NONE => (ys, x) | SOME (y, x) => lp (y::ys) x
   in
      lp []
   end
   fun unfoldr' f = Pair.map (rev, Fn.id) o unfoldl' f
   fun unfoldl f = #1 o unfoldl' f
   fun unfoldr f = #1 o unfoldr' f
   fun intersperse d =
       fn [] => [] | x::xs => x::foldr (fn (x, ys) => d::x::ys) [] xs
   local
      fun headsAndTails xss =
          Pair.map (rev, rev)
                   (foldl (fn (h::t, (hs, ts)) => (h::hs, t::ts) | ([], ?) => ?)
                          ([], []) xss)
   in
      fun transpose xss = let
         fun lp yss =
             fn [] => rev yss
              | []::xss => lp yss xss
              | (x::xs)::xss => let
                   val (hs, ts) = headsAndTails xss
                in
                   lp ((x::hs)::yss) (xs::ts)
                end
      in
         lp [] xss
      end
   end
   fun foldl1 f = fn [] => raise Empty | x::xs => foldl f x xs
   fun foldr1 f = foldl1 f o rev
   fun push r x = r := x :: !r
   fun pop r = case !r of x::xs => (r := xs ; SOME x) | [] => NONE
   fun split (l, i) =
       if i < 0 then raise Subscript
       else Pair.map (Fn.id, #1)
                     (unfoldr' (fn (_, 0) => NONE
                                 | ([], _) => raise Subscript
                                 | (x::xs, n) => SOME (x, (xs, n-1)))
                               (l, i))
   fun findi p = let
      fun lp i =
       fn [] => NONE
        | x::xs => if p (i, x) then SOME (i, x) else lp (i+1) xs
   in
      lp 0
   end
   fun equal eq = let
      fun lp ([],       []) = true
        | lp (x::xs, y::ys) = eq (x, y) andalso lp (xs, ys)
        | lp (_,         _) = false
   in
      lp
   end
   fun concatMap f = rev o foldl (revAppend o Pair.map (f, Fn.id)) []
   fun appr e = app e o rev
   fun foldli f y = #2 o foldl (fn (x, (i, y)) => (i+1, f (i+1, x, y))) (~1, y)
   fun foldri f y xs = let
      val (n, xs) = foldl (fn (x, (n, xs)) => (n+1, x::xs)) (0, []) xs
   in
      #2 (foldl (fn (x, (i, y)) => (i-1, f (i-1, x, y))) (n, y) xs)
   end
   fun concatMapi f =
       rev o foldli (fn (i, x, ys) => revAppend (f (i, x), ys)) []
   fun mapiPartial f =
       rev o foldli (fn (i, x, ys) => case f (i, x) of NONE => ys
                                                     | SOME y => y::ys) []
   fun mapi f = mapiPartial (SOME o f)
   fun appi e = foldli (fn (i, x, ()) => e (i, x)) ()
   fun appri e = foldri (fn (i, x, ()) => e (i, x)) ()
   fun existsi p = Option.isSome o findi p
   fun alli p = Option.isNone o findi (not o p)
   fun index ? = mapi Fn.id ?
   fun contains l x = exists (fn y => x = y) l
   fun maximum cmp = foldl1 (Cmp.max cmp o Pair.swap)
   fun minimum cmp = foldl1 (Cmp.min cmp o Pair.swap)
   local
      fun mk combine init pred = let
         fun lp ts =
          fn [] => (ts, [])
           | x::xs => if pred x then lp (combine (x, ts)) xs else (ts, x::xs)
      in
         lp init
      end
   in
      fun span ? = Pair.map (rev, Fn.id) o mk op :: [] ?
      fun takeWhile ? = rev o #1 o mk op :: [] ?
      fun dropWhile ? = #2 o mk ignore () ?
   end
   fun process f s = fn [] => s | x::xs => Fn.uncurry (process f) (f (x, s, xs))
   fun uniqueByEq eq =
    fn [] => true
     | x::xs => not (exists (Fn.curry eq x) xs) andalso uniqueByEq eq xs
   fun tails l = rev ([]::process (fn (x, a, xs) => ((x::xs)::a, xs)) [] l)
   fun inits l = []::process (fn (x, a, xs) => (rev (x::xs)::a, xs)) [] (rev l)
   fun divideByEq eq =
       rev o process (fn (y, cs, xs) => let
                            val (ys, xs) = partition (Fn.curry eq y) xs
                         in
                            ((y::ys)::cs, xs)
                         end) []
   fun nubByEq eq =
       rev o foldl (fn (x, ys) =>
                       if exists (Fn.curry eq x) ys then ys else x::ys) []
end
