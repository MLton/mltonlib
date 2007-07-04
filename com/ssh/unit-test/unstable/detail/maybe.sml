(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A small combinator library for specifying queries.
 *
 * This is similar to the Maybe monad familiar from Haskell, but we can,
 * of course, also perform effectful queries.  An example of an effectful
 * query is {E} which queries the environment.
 *)
structure Maybe :> sig
   type 'v t
   include MONADP_CORE where type 'v monad = 'v t
   structure Monad : MONADP where type 'v monad = 'v t
   val ` : 'a -> 'a t
   val liftBinFn : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t (* XXX move to MONAD *)
   val get : 'a t -> 'a Option.t
   val mk : ('k -> 'v Option.t) -> 'k -> 'v t
   val E : String.t -> String.t t
   val ^` : String.t t BinOp.t
   val @` : 'a t * ('a -> 'b Option.t) -> 'b t
   val O : String.t -> Unit.t t
   val L : String.t -> String.t t
   val S : String.t -> String.t t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix >>= <|> >>*
   infixr |< @`
   (* SML/NJ workaround --> *)

   type 'v t = 'v Option.t Thunk.t
   fun ` x = const (SOME x)
   structure Monad =
     MkMonadP
       (type 'v monad = 'v t
        val return = `
        fun (aM >>= a2bM) () = case aM () of NONE => NONE | SOME a => a2bM a ()
        fun zero () = NONE
        fun (l <|> r) () = case l () of NONE => r () | r => r)
   open Monad
   fun liftBinFn f (aM, bM) = map f (aM >>* bM)
   fun get q = q ()
   fun mk f k () = f k
   val E = mk OS.Process.getEnv
   val op ^` = liftBinFn op ^
   local
      fun is s x = s = x
      fun isE s = String.isPrefix (s^"=")
      fun two f s = fn a::x::_ => SOME (f (s, a, x)) | _ => NONE
      fun one f s = fn [] => NONE | x::_ => SOME (f (s, x))
      val drop = flip List.dropWhile (CommandLine.arguments ())
      fun arg p r e = mk (fn s => r e s o drop |< not o p s)
   in
      val L = arg isE one (fn (s, a) => String.extract (a, 1+size s, NONE))
      val S = arg is two #3
      val O = arg is one (const ())
   end
   fun aM @` from = aM >>= const o from
end
