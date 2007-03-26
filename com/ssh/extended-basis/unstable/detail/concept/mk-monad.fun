(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonad (Core : MONAD_CORE) : MONAD = struct
   infix >> >>& >>* >>= >>@ oo
   open Core
   type 'a func = 'a monad
   fun map f aM = aM >>= return o f
   type 'a monad_ex = 'a monad
   fun aM >>* bM = aM >>= (fn a => bM >>= Fn.<\ (a, return))
   fun fM >>@ aM = map Fn.\> (fM >>* aM)
   fun aM >>& bM = map Product.& (aM >>* bM)
   fun aM >> bM = map #2 (aM >>* bM)

   fun pure f = return o f
   fun thunk thk = return () >>= pure thk

   fun ignore m = m >> return ()
   fun y2zM oo x2yM = (fn x => x2yM x >>= y2zM)

   local
      fun mkFold fM b fin =
       fn [] => return (fin b)
        | x::xs => fM (x, b) >>= (fn b' => mkFold fM b' fin xs)
   in
      fun foldl fM b = mkFold fM b Fn.id
      fun foldr fM b = (foldl fM b) o List.rev

      fun seqWith x2yM = mkFold (fn (x, xs') => x2yM x >>= (fn x' => return (x'::xs'))) [] List.rev
      fun appWith x2yM = foldl (ignore o x2yM o Pair.fst) ()

      fun seq xMs = seqWith Fn.id xMs
      fun app xMs = appWith Fn.id xMs

      fun seqWithPartial x2yM = 
       mkFold (fn (x, xs') => x2yM x >>= (fn SOME x' => return (x'::xs') | NONE => return xs')) [] List.rev
   end

   fun when b m = if b then m else return ()
   fun unless b m = if b then return () else m

   local 
     fun tabulateTail f n m ac =
       if n = m then 
         return (List.rev ac)
       else 
         f m >>= (fn x => tabulateTail f n (m + 1) (x::ac))
   in
     fun tabulate n f = tabulateTail f n 0 [] 
   end

   local
     fun pairFst x y = (y, x)
     fun pairSnd x y = (x, y) 
   in
     fun mapFst x2yM (x, z) = x2yM x >>= (pure o pairFst) z 
     fun mapSnd x2yM (z, x) = x2yM x >>= (pure o pairSnd) z
   end

end

functor MkMonadP (Core : MONADP_CORE) : MONADP = struct
   infix <|>
   structure Monad = MkMonad (Core)
   open Monad Core
   type 'a monadp_ex = 'a monad

   fun sumWith x2yM = 
       fn [] => zero 
        | [x] => x2yM x
        | x::xs => x2yM x <|> sumWith x2yM xs

   fun sum ms = sumWith Fn.id ms 

end
