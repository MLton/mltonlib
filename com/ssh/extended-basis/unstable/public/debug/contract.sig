(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * The {Contract} module provides a combinator library for specifying
 * contrants.  Inspiration comes mainly from the article:
 *
 *   Contracts for Higher-Order Functions
 *   Robert Bruce Findler and Matthias Felleisen
 *   ICFP 2002
 *   [http://citeseer.ist.psu.edu/findler02contracts.html]
 *
 * Another combinator library with the same source of inspiration, but a
 * different implementation, is described in the article:
 *
 *   Typed Contracts for Functional Programming
 *   Ralf Hinze, Johan Jeuring, and Andres Löh
 *   FLOPS 2006
 *   [http://people.cs.uu.nl/andres/Contracts.html]
 *)
signature CONTRACT = sig
   type 'a t
   exception Contract
   exception Caller of Exn.t
   exception Callee of Exn.t
   val assert : 'a t -> 'a UnOp.t
   val T : 'a t
   val F : 'a t
   val ef : 'a Effect.t -> 'a t
   val pr : 'a UnPr.t -> 'a t
   val andAlso : 'a t BinOp.t
   val --> : 'a t * ('a -> 'b t) -> ('a -> 'b) t
end
