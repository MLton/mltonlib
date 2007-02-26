(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature MONAD_CORE = sig
   type 'a monad
   val return : 'a -> 'a monad
   val >>= : 'a monad * ('a -> 'b monad) -> 'b monad
end

signature MONAD_EX = sig
   type 'a monad_ex
   include FUNC where type 'a func = 'a monad_ex
   val >> : 'a monad_ex * 'b monad_ex -> 'b monad_ex
   val >>& : 'a monad_ex * 'b monad_ex -> ('a, 'b) Product.t monad_ex
   val >>* : 'a monad_ex * 'b monad_ex -> ('a * 'b) monad_ex
   val >>@ : ('a -> 'b) monad_ex * 'a monad_ex -> 'b monad_ex
   val seq : 'a monad_ex List.t -> 'a List.t monad_ex
end

signature MONAD = sig
   include MONAD_CORE
   include MONAD_EX where type 'a monad_ex = 'a monad
end

signature MONADP_CORE = sig
   include MONAD_CORE
   val zero : 'a monad
   val plus : 'a monad BinOp.t
end

signature MONADP_EX = sig
   type 'a monadp_ex
   val sum : 'a monadp_ex List.t -> 'a monadp_ex
end

signature MONADP = sig
   include MONADP_CORE
   include MONAD_EX where type 'a monad_ex = 'a monad
   include MONADP_EX where type 'a monadp_ex = 'a monad
end

(************************************************************************)

signature MONAD_CORE' = sig
   type ('a, 'x) monad
   val return : 'a -> ('a, 'x) monad
   val >>= : ('a, 'x) monad * ('a -> ('b, 'x) monad) -> ('b, 'x) monad
end

signature MONAD_EX' = sig
   type ('a, 'x) monad_ex
   include FUNC' where type ('a, 'x) func = ('a, 'x) monad_ex
   val >> : ('a, 'x) monad_ex * ('b, 'x) monad_ex -> ('b, 'x) monad_ex
   val >>* : ('a, 'x) monad_ex * ('b, 'x) monad_ex -> ('a * 'b, 'x) monad_ex
   val >>& : ('a, 'x) monad_ex * ('b, 'x) monad_ex
             -> (('a, 'b) Product.t, 'x) monad_ex
   val >>@ : ('a -> 'b, 'x) monad_ex * ('a, 'x) monad_ex -> ('b, 'x) monad_ex
   val seq : ('a, 'x) monad_ex List.t -> ('a List.t, 'x) monad_ex
end

signature MONAD' = sig
   include MONAD_CORE'
   include MONAD_EX' where type ('a, 'x) monad_ex = ('a, 'x) monad
end

signature MONADP_CORE' = sig
   include MONAD_CORE'
   val zero : ('a, 'x) monad
   val plus : ('a, 'x) monad BinOp.t
end

signature MONADP_EX' = sig
   type ('a, 'x) monadp_ex
   val sum : ('a, 'x) monadp_ex List.t -> ('a, 'x) monadp_ex
end

signature MONADP' = sig
   include MONADP_CORE'
   include MONAD_EX' where type ('a, 'x) monad_ex = ('a, 'x) monad
   include MONADP_EX' where type ('a, 'x) monadp_ex = ('a, 'x) monad
end
