(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the layered representation types of generics.
 *)
signature LAYERED_GENERIC_REP = sig
   structure Outer : OPEN_GENERIC_REP
   structure Closed : CLOSED_GENERIC_REP
   structure Inner : sig
      include OPEN_GENERIC_REP
      val mkT :  'a      Closed.t * 'x -> ('a,     'x) t
      val mkS :  'a      Closed.s * 'x -> ('a,     'x) s
      val mkP : ('a, 'k) Closed.p * 'x -> ('a, 'k, 'x) p

      val mkY : 'a Closed.t Tie.t * 'x Tie.t -> ('a, 'x) t Tie.t
   end
   include OPEN_GENERIC_REP
      where type ('a,     'x) t = ('a,     ('a,     'x) Inner.t) Outer.t
      where type ('a,     'x) s = ('a,     ('a,     'x) Inner.s) Outer.s
      where type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k, 'x) Inner.p) Outer.p
   structure This : sig
      val getT : ('a,     'x) t ->  'a      Closed.t
      val getS : ('a,     'x) s ->  'a      Closed.s
      val getP : ('a, 'k, 'x) p -> ('a, 'k) Closed.p

      val mapT :  'a      Closed.t UnOp.t -> ('a,     'x) t UnOp.t
      val mapS :  'a      Closed.s UnOp.t -> ('a,     'x) s UnOp.t
      val mapP : ('a, 'k) Closed.p UnOp.t -> ('a, 'k, 'x) p UnOp.t
   end
end
