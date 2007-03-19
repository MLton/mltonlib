(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonad (Core : MONAD_CORE) : MONAD = struct
   infix >> >>& >>* >>= >>@
   open Core
   type 'a func = 'a monad
   fun map f aM = aM >>= return o f
   type 'a monad_ex = 'a monad
   fun aM >>* bM = aM >>= (fn a => bM >>= Fn.<\ (a, return))
   fun fM >>@ aM = map Fn.\> (fM >>* aM)
   fun aM >>& bM = map Product.& (aM >>* bM)
   fun aM >> bM = map #2 (aM >>* bM)
   local
      fun mk fin comb x2yM ac =
       fn [] => return (fin ac)
        | x::xs => x2yM x >>= (fn y => mk fin comb x2yM (comb (y, ac)) xs)
   in
      fun seqWith x2yM = mk rev op :: x2yM []
      fun appWith x2yM = mk ignore ignore x2yM ()
      fun seq xMs = seqWith Fn.id xMs
      fun app xMs = appWith Fn.id xMs
   end
   fun ignore m = m >> return ()
   fun when b m = if b then m else return ()
   fun unless b m = if b then return () else m
end

functor MkMonadP (Core : MONADP_CORE) : MONADP = struct
   infix <|>
   structure Monad = MkMonad (Core)
   open Monad Core
   type 'a monadp_ex = 'a monad
   fun sum [] = zero | sum [x] = x | sum (x::xs) = x <|> sum xs
end
