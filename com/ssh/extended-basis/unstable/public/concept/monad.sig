(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Monad ==
 *
 * The concept of a monad comes from category theory.  Here a monad
 * consists of a type constructor {monad} and two functions {return} and
 * {>>=} (pronounced "bind"):
 *
 *> type 'a monad
 *> val return : 'a -> 'a monad
 *> val >>= : 'a monad * ('a -> 'b monad) -> 'b monad
 *
 * Furthermore, the {return} and {>>=} functions must obey three laws:
 *
 *> 1. return x >>= f == f x
 *> 2. m >>= return == m
 *> 3. (m >>= f) >>= g == m >>= (fn x => f x >>= g)
 *
 * The first two laws basically say that {return} is both a left and
 * right-identity with respect to {>>=}.  The third law basically says
 * that {>>=} is associative.
 *)

signature MONAD_CORE = sig
   type 'a monad
   val return : 'a -> 'a monad
   val >>= : 'a monad * ('a -> 'b monad) -> 'b monad
end

signature MONAD_EX = sig
   type 'a monad_ex
   include FUNC where type 'a func = 'a monad_ex
   val =<< : ('a -> 'b monad_ex) * 'a monad_ex -> 'b monad_ex
   val >> : 'a monad_ex * 'b monad_ex -> 'b monad_ex
   val >>& : 'a monad_ex * 'b monad_ex -> ('a, 'b) Product.t monad_ex
   val >>* : 'a monad_ex * 'b monad_ex -> ('a * 'b) monad_ex
   val >>@ : ('a -> 'b) monad_ex * 'a monad_ex -> 'b monad_ex

   val pure : ('a -> 'b) -> 'a -> 'b monad_ex
   (** {pure f == return o f} *)

   val thunk : 'a Thunk.t -> 'a monad_ex 
   (** {thunk thk == return () >>= pure thunk} *)

   val seq : 'a monad_ex List.t -> 'a List.t monad_ex
   val seqWith : ('a -> 'b monad_ex) -> 'a List.t -> 'b List.t monad_ex
   val seqWithPartial : ('a -> 'b Option.t monad_ex) -> 'a List.t -> 
                        'b List.t monad_ex

   val app : 'a monad_ex List.t -> unit monad_ex
   val appWith : ('a -> 'b monad_ex) -> 'a List.t -> unit monad_ex

   val oo : ('b -> 'c monad_ex) * ('a -> 'b monad_ex) -> 'a -> 
            'c monad_ex
   (** {f2 oo f1 == (fn x => f1 x >>= f2) } *)

   val ignore : 'a monad_ex -> unit monad_ex
   (** {ignore m == (m >> return ())} *)

   val when : bool -> unit monad_ex -> unit monad_ex
   (** {when b m == if b then m else (return ())} *)

   val unless : bool -> unit monad_ex -> unit monad_ex
   (** {unless b m == if b then (return ()) else m} *)

   val tabulate : int -> (int -> 'a monad_ex) -> 'a List.t monad_ex
   (**
     * Tabulate is a version of List.tabulate that can use
     * functions that produce computations.  
     *
     * {tabulate n f == 
     *   (f 0) >>= (fn x0 => (f 1) >>= ... 
     *                (fn xn >>= return [x1, ..., xn]))} 
     *
     * The actual implementation is tail recursive. *) 

  val foldl : ('a * 'b -> 'b monad_ex) -> 'b -> 'a list -> 'b monad_ex 
  val foldr : ('a * 'b -> 'b monad_ex) -> 'b -> 'a list -> 'b monad_ex 

  val mapFst : ('a -> 'c monad_ex) -> ('a, 'b) Pair.t -> 
               ('c, 'b) Pair.t monad_ex
  val mapSnd : ('b -> 'c monad_ex) -> ('a, 'b) Pair.t -> 
               ('a, 'c) Pair.t monad_ex

end

signature MONAD = sig
   include MONAD_CORE MONAD_EX
   sharing type monad_ex = monad
end

(** == Monad Plus == *)

signature MONADP_CORE = sig
   include MONAD_CORE
   val zero : 'a monad
   val <|> : 'a monad BinOp.t
end

signature MONADP_EX = sig
   type 'a monadp_ex
   val sum : 'a monadp_ex List.t -> 'a monadp_ex
   val sumWith : ('a -> 'b monadp_ex) -> 'a List.t -> 'b monadp_ex
end

signature MONADP = sig
   include MONADP_CORE MONAD_EX MONADP_EX
   sharing type monad = monad_ex = monadp_ex
end

(** == State Monads == *)

signature MONAD_WS = sig (* WS = WITH_STATE *)
  type 'a monad_ws
  type monad_ws_state
  val getState : monad_ws_state monad_ws
  val setState : monad_ws_state -> Unit.t monad_ws
  val run : monad_ws_state -> 'a monad_ws -> monad_ws_state * 'a
end 
 
signature MONAD_STATE = sig
  include MONAD MONAD_WS
  sharing type monad = monad_ws
end 

signature MONADP_STATE = sig
  include MONADP MONAD_WS
  sharing type monad = monad_ws
end 
