(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonad (MonadCore : MONAD_CORE) : MONAD = struct
   infix >> >>& >>* >>= >>@
   open MonadCore
   type 'a func = 'a monad
   fun map f aM = aM >>= return o f
   type 'a monad_ex = 'a monad
   fun aM >>* bM = aM >>= (fn a => bM >>= Fn.<\ (a, return))
   fun fM >>@ aM = map Fn.\> (fM >>* aM)
   fun aM >>& bM = map Product.& (aM >>* bM)
   fun aM >> bM = map #2 (aM >>* bM)
   fun seq [] = return []
     | seq (xM::xMs) = map op :: (xM >>* seq xMs)

   local 
     fun seqWithTail f xs accum =
         case xs
           of [] => return (List.rev accum)
            | x::xs' => (f x) >>= (fn x' => seqWithTail f xs' (x'::accum))
   in
     fun seqWith f xs =
         seqWithTail f xs []
   end

   fun app (ms : 'a monad list) : unit monad =
       case ms
         of [] => return ()
          | m::ms' => m >> (app ms')

   fun appWith (f : 'a -> 'b monad) (xs : 'a list) : unit monad =
       case xs
         of [] => return ()
          | x::xs' => (f x) >> (appWith f xs')

   fun ignore m = m >> return ()

   fun when b m = if b then m else return ()
   fun unless b m = if b then return () else m

end

functor MkMonadP (MonadPCore : MONADP_CORE) : MONADP = struct
   structure Monad = MkMonad (MonadPCore)
   open Monad MonadPCore
   type 'a monadp_ex = 'a monad
   fun sum [] = zero | sum [x] = x | sum (x::xs) = plus (x, sum xs)
end
