(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Signature for iterator or loop combinators. *)
signature ITER = sig
   type 'a t = ('a, Unit.t) CPS.t
   (** The type of iterator functions. *)

   (** == Running Iterators == *)

   val for : 'a t -> ('a, Unit.t) CPS.t
   (**
    *> for [<>]                f = ()
    *> for [<x(0), x(1), ...>] f = (f x(0) ; for [<x(1), ...>] f)
    *
    * This is actually the identity function and is provided purely for
    * syntactic sugar.
    *)

   val fold : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   (**
    *> fold f s [<>]                      = s
    *> fold f s [<x(0), x(1), ..., x(n)>] =
    *>    fold f (f (x(0), s)) [<x(1), ..., x(n)>]
    *)

   val find : 'a UnPr.t -> 'a t -> 'a Option.t
   (**
    *> find p [<>]                = NONE
    *> find p [<x(0), x(1), ...>] =
    *>    if p x(0) then SOME x(n) else find p [<x(1), ...>]
    *)

   val reduce : 'b -> 'b BinOp.t -> ('a -> 'b) -> 'a t -> 'b
   (** {reduce zero plus one = fold plus zero o Monad.map one} *)

   val collect : 'a t -> 'a List.t
   (** {collect [<x(0), x(1), ..., x(n)>] = [x(0), x(1), ..., x(n)]} *)

   (** == Combinators == *)

   include MONADP_CORE where type 'a monad = 'a t
   structure Monad : MONADP where type 'a monad = 'a t

   val unfold : ('a, 's) Reader.t -> 's -> 'a t
   (**
    *> unfold g s f =
    *>    case g s of NONE        => ()
    *>              | SOME (x, s) => (f x ; unfold g s f)
    *)

   val until : 'a t * 'a UnPr.t -> 'a t
   (**
    * {[<x(0), x(1), ...>] until p = [<x(0), x(1), ..., x(n)>]} where {p
    * x(i) = false} for all {0<=i<=n} and {p x(n+1) = true}.
    *)

   val index : 'a t -> ('a, Int.t) Product.t t
   (** {index [<x(0), x(1), ...>] = [<x(0) & 0, x(1) & 1, ...>]} *)

   val iterate : 'a UnOp.t -> 'a -> 'a t
   (** {iterate f x = [<x, f x, f (f x), ...>]} *)

   val when : 'a t * 'a UnPr.t -> 'a t

   val by : 'a t * ('a -> 'b) -> 'b t
   (**
    *> [<x(0), x(1), ...>] by f = [<f x(0), f x(1), ...>]
    *
    * {s by f} is the same as {Monad.map f s}.
    *)

   val >< : 'a t * 'b t -> ('a, 'b) Product.t t
   (**
    *> [<x(0), x(1), ...>] >< [<y(0), y(1), ..., y(n)>] =
    *>    [<x(0) & y(0), x(0) & y(1), ..., x(0) & y(n),
    *>      x(1) & y(0), x(1) & y(1), ..., x(1) & y(n),
    *>      ...>]
    *
    * This is the same as {Monad.><}.
    *)

   (** == Iterating over Integers == *)

   val up : Int.t -> Int.t t
   (** {up l = [<l, l+1, ...>]} *)

   val upTo : Int.t -> Int.t -> Int.t t
   (** {upTo l u = [<l, l+1, ..., u-1>]} *)

   val upToBy : Int.t -> Int.t -> Int.t -> Int.t t
   (** {upToBy l u d = [<l+0*d, l+1*d, ..., l + (u-l) div d * d>]} *)

   val down : Int.t -> Int.t t
   (** {down u = [<u-1, u-2, ...>]} *)

   val downTo : Int.t -> Int.t -> Int.t t
   (** {downTo u l = [<u-1, u-2, ..., l>]} *)

   val downToBy : Int.t -> Int.t -> Int.t -> Int.t t
   (** {downToBy u l d = [<u-1*d, u-2*d, ..., u - (u-l+d-1) div d * d>]} *)

   (** == Iterators Over Standard Sequences == *)

   val inList : 'a List.t -> 'a t

   val inArray : 'a Array.t -> 'a t
   val inArraySlice : 'a ArraySlice.t -> 'a t
   val inVector : 'a Vector.t -> 'a t
   val inVectorSlice : 'a VectorSlice.t -> 'a t

   val inCharArray : CharArray.t -> Char.t t
   val inCharArraySlice : CharArraySlice.t -> Char.t t
   val inCharVector : CharVector.t -> Char.t t
   val inCharVectorSlice : CharVectorSlice.t -> Char.t t
   val inString : String.t -> Char.t t
   val inSubstring : Substring.t -> Char.t t
   val inWord8Array : Word8Array.t -> Word8.t t
   val inWord8ArraySlice : Word8ArraySlice.t -> Word8.t t
   val inWord8Vector : Word8Vector.t -> Word8.t t
   val inWord8VectorSlice : Word8VectorSlice.t -> Word8.t t
end
