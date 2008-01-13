(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with functions. *)
signature FN = sig
   type ('a, 'b) t = 'a -> 'b
   (** The type of functions. *)

   val const : 'a -> 'b -> 'a
   (** K-combinator ({const x y = x}). *)

   val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   (** Currying ({curry f x y = f (x, y)}). *)

   val eta : ('a -> 'b -> 'c) UnOp.t
   (**
    * {eta f x} is equivalent to {case (f, x) of (f, x) => fn y => f x y}.
    * In other words, {eta} delays function application.
    *)

   val fix : ('a -> 'b) Fix.t
   (** Fixpoint of given functional. *)

   val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
   (** Flip the order of arguments ({flip f x y = f y x}). *)

   val id : 'a -> 'a
   (** I-combinator ({id x = x}). *)

   val iso : ('a, 'c) Iso.t * ('b, 'd) Iso.t -> (('a, 'b) t, ('c, 'd) t) Iso.t
   (** Lifts isos between elements to an iso between arrows. *)

   val map : ('c -> 'a) * ('b -> 'd) -> ('a -> 'b) -> 'c -> 'd
   (** {map (f, g) h = g o h o f}. *)

   val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   (** Function composition ({(g o f) x = f (g x)}). *)

   val seal : ('a -> 'b) -> 'a -> 'b Thunk.t
   (**
    * {seal f x} is equivalent to {fn () => f x} assuming {f} and {x} are
    * variables.
    *)

   val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
   (** Uncurrying ({uncurry f (x, y) = f x y}). *)

   val <\ : 'a * ('a * 'b -> 'c) -> 'b -> 'c
   (** Left section ({(x <\ f) y = f (x, y)}). *)

   val \> : ('a -> 'b) * 'a -> 'b
   (**
    * Left application ({f \> x1 \> ... \> xN = f x1 ... xN}) and infix
    * ({x0 <\f1\> x1 ... <\fN\> xN = fN (... f1 (x0, x1) ..., xN)}).
    *)

   val /> : ('a * 'b -> 'c) * 'b -> 'a -> 'c
   (** Right section ({(f /> y) x = f (x, y)}). *)

   val </ : 'a * ('a -> 'b) -> 'b
   (**
    * Right application ({xN </ ... </ x1 </ f = f x1 ... xN}) and infix
    * ({xN </fN/> ... x1 </f1/> x0 = fN (xN, ... f1 (x1, x0) ...)}).
    *)

   val >| : 'a * ('a -> 'b) -> 'b
   (** Left pipe ({x >| f1 >| ... >| fN = (fN o ... o f1) x}). *)

   val |< : ('a -> 'b) * 'a -> 'b
   (** Right pipe ({fN |< ... |< f1 |< x = (fN o ... o f1) x}). *)
end
