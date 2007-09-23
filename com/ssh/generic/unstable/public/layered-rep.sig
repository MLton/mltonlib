(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the layered representation types of generics.
 *)
signature LAYERED_REP = sig
   structure Outer : OPEN_REP
   structure Inner : OPEN_REP
   include OPEN_REP
      where type ('a,     'x) t = ('a,     ('a,     'x) Inner.t) Outer.t
      where type ('a,     'x) s = ('a,     ('a,     'x) Inner.s) Outer.s
      where type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k, 'x) Inner.p) Outer.p
   structure This : sig
      include CLOSED_REP

      val getT : ('a,     ('a,     'x) Inner.t) Outer.t ->  'a      t
      val getS : ('a,     ('a,     'x) Inner.s) Outer.s ->  'a      s
      val getP : ('a, 'k, ('a, 'k, 'x) Inner.p) Outer.p -> ('a, 'k) p

      val mapT :  'a      t UnOp.t -> ('a,     ('a,     'x) Inner.t) Outer.t UnOp.t
      val mapS :  'a      s UnOp.t -> ('a,     ('a,     'x) Inner.s) Outer.s UnOp.t
      val mapP : ('a, 'k) p UnOp.t -> ('a, 'k, ('a, 'k, 'x) Inner.p) Outer.p UnOp.t

      val mkT :  'a      t * 'x -> ('a,     'x) Inner.t
      val mkS :  'a      s * 'x -> ('a,     'x) Inner.s
      val mkP : ('a, 'k) p * 'x -> ('a, 'k, 'x) Inner.p

      val mkY : 'a t Tie.t * 'x Tie.t -> ('a, 'x) Inner.t Tie.t
   end
end
