(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with functions. *)
signature FN = sig
   type ('a, 'b) t = 'a -> 'b
   (** The type of functions. *)

   val map : ('c -> 'a) * ('b -> 'd) -> ('a -> 'b) -> 'c -> 'd
   (** {map (f, g) h = g o h o f}. *)

   val const : 'a -> 'b -> 'a
   (** K-combinator ({const x y = x}). *)

   val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   (** Currying ({curry f x y = f (x, y)}). *)

   val failing : exn -> 'a -> 'b
   (**
    * A failing function; {failing e} is equivalent to {fn _ => raise e},
    * assuming {e} is a variable.
    *)

   val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
   (** Flip the order of arguments ({flip f x y = f y x}). *)

   val id : 'a -> 'a
   (** I-combinator ({id x = x}). *)

   val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
   (** Uncurrying ({uncurry f (x, y) = f x y}). *)

   val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   (** Function composition ({(g o f) x = f (g x)}). *)

   val undefined : 'a -> 'b
   (**
    * An undefined function.  This is equivalent to {failing (Fail
    * "undefined")}.
    *)

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