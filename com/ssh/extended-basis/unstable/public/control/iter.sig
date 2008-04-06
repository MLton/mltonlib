(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Signature for iterator or loop combinators. *)
signature ITER = sig
   type 'a t = 'a Effect.t Effect.t
   (** The type of iterator functions. *)

   (** == Running Iterators == *)

   val all : 'a UnPr.t -> 'a t UnPr.t
   (**
    *> all p [<>]                = true
    *> all p [<x(0), x(1), ...>] = p x(0) andalso all p [<x(1), ...>]
    *
    *> all = neg o exists o neg
    *)

   val collect : 'a t -> 'a List.t
   (** {collect [<x(0), x(1), ..., x(n)>] = [x(0), x(1), ..., x(n)]} *)

   val exists : 'a UnPr.t -> 'a t UnPr.t
   (**
    *> exists p [<>]                = false
    *> exists p [<x(0), x(1), ...>] = p x(0) orelse exists p [<x(1), ...>]
    *
    *> exists = neg o all o neg
    *)

   val find : 'a UnPr.t -> 'a t -> 'a Option.t
   (**
    *> find p [<>]                = NONE
    *> find p [<x(0), x(1), ...>] =
    *>    if p x(0) then SOME x(n) else find p [<x(1), ...>]
    *)

   val first : 'a t -> 'a Option.t
   (**
    *> first [<>]                = NONE
    *> first [<x(0), x(1), ...>] = SOME x(0)
    *
    * Only the first element, if any, of the iterator will be computed.
    *)

   val fold : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   (**
    *> fold f s [<>]                      = s
    *> fold f s [<x(0), x(1), ..., x(n)>] =
    *>    fold f (f (x(0), s)) [<x(1), ..., x(n)>]
    *)

   val for : 'a t -> 'a Effect.t Effect.t
   (**
    *> for [<>]                f = ()
    *> for [<x(0), x(1), ...>] f = (f x(0) ; for [<x(1), ...>] f)
    *
    * This is actually the identity function and is provided purely for
    * syntactic sugar.
    *)

   val last : 'a t -> 'a Option.t
   (**
    *> first [<>]                      = NONE
    *> first [<x(0), x(1), ..., x(n)>] = SOME x(n)
    *
    * Note that all elements of the iterator will be computed.
    *)

   val reduce : 'b -> 'b BinOp.t -> ('a -> 'b) -> 'a t -> 'b
   (** {reduce zero plus one = fold plus zero o Monad.map one} *)

   (** == Monad ==
    *
    * Iterators essentially form a monad with plus and satisfy the same
    * laws, notably the left distribution law, as the (lazy) List monad.
    *
    * Monad {zero} is iterator for the empty sequence:
    *
    *> zero = [<>]
    *
    * Monad {<|>} (plus) is iterator concatenation:
    *
    *> [<a(0), a(1), ...>]       <|> [<b(0), b(1), ...>] = [<a(0), a(1), ...>]
    *> [<a(0), a(1), ..., a(n)>] <|> [<b(0), b(1), ...>] =
    *>    [<a(0), a(1), ..., a(n)>, b(0), b(1), ...]
    *
    * Monad {return} is an iterator for the singleton sequence:
    *
    *> return x = [<x>]
    *
    * Monad {>>=} (bind) is mapping and concatenation ({concatMap}) of
    * iterators:
    *
    *> [<a(0), a(1), ...>] >>= a2bI = a2bI a(0) <|> a2bI a(1) <|> ...
    *)

   include MONADP_CORE where type 'a monad = 'a t
   structure Monad : MONADP where type 'a monad = 'a t

   (** == Unfolding == *)

   val unfold : ('a, 's) Reader.t -> 's -> 'a t
   (**
    *> unfold g s f =
    *>    case g s of NONE        => ()
    *>              | SOME (x, s) => (f x ; unfold g s f)
    *)

   val iterate : 'a UnOp.t -> 'a -> 'a t
   (** {iterate f x = [<x, f x, f (f x), ...>]} *)

   (** == Frequently used Monad Combinators ==
    *
    * These combinators are copied from the {Monad} substructure, because
    * they are frequently useful.
    *)

   val filter : 'a UnPr.t -> 'a t UnOp.t
   (**
    *> filter p [<x(0), x(1), ...>] =
    *>    (if p x(0) then [<x(0)>] else [<>]) <|> filter p [<x(1), ...>]
    *
    *> fun filter p m = m >>= (fn x => if p x then return x else zero)
    *
    * This is the same as {Monad.filter}.
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

   (** == Misc Combinators == *)

   val intersperse : 'a -> 'a t UnOp.t
   (** {intersperse x [<a(0), a(1), ...>] = [<a(0), x, a(1), x, ...>]} *)

   val on : 'a t -> 'a Effect.t -> 'a t
   (** Apply effect on each element of the iterator. *)

   (** == Repetition == *)

   val repeat : 'a -> 'a t
   (** {repeat x = [<x, x, ...>]} *)

   val replicate : Int.t -> 'a -> 'a t
   (** {replicate n x = [<x(1), x(2), ..., x(n)>]} *)

   val cycle : 'a t UnOp.t
   (**
    *> cycle [<x(0), x(1), ..., x(n)>] =
    *>    [<x(0), x(1), ..., x(n),
    *>      x(0), x(1), ..., x(n),
    *>      ...>]
    *)

   (** == Stopping Early == *)

   val take : Int.t -> 'a t UnOp.t
   (**
    *> take n [<x(0), x(1), ..., x(m)>] = [<x(0), x(1), ..., x(m)>], m < n
    *> take n [<x(0), x(1), ..., x(n-1), ...>] = [<x(0), x(1), ..., x(n-1)>]
    *)

   val until : 'a UnPr.t -> 'a t UnOp.t
   (**
    * {until p [<x(0), x(1), ...>] = [<x(0), x(1), ..., x(n)>]} where {p
    * x(i) = false} for all {0<=i<=n} and {p x(n+1) = true}.
    *)

   val until' : 'a UnPr.t -> 'a t UnOp.t
   (**
    * {until' p [<x(0), x(1), ...>] = [<x(0), x(1), ..., x(n)>]} where {p
    * x(i) = false} for all {0<=i<n} and {p x(n) = true}.
    *)

   val whilst : 'a UnPr.t -> 'a t UnOp.t
   (** {whilst = until o neg} *)

   val whilst' : 'a UnPr.t -> 'a t UnOp.t
   (** {whilst' = until' o neg} *)

   (** == Optional Argument Modifiers ==
    *
    * The following modifiers are used to specify additional optional
    * arguments to a number of iterators.  They are optional and can
    * be specified in any order.  The default value, when a modifier is
    * absent, depends on the iterator.
    *)

   type ('f, 't, 'b) mod

   val From : ('f,
               (('x, 't, 'b) mod, 'd, 'r) Fold.t,
               (('f, 't, 'b) mod, 'd, 'r) Fold.t, 'k) Fold.s1

   val To : ('t,
             (('f, 'x, 'b) mod, 'd, 'r) Fold.t,
             (('f, 't, 'b) mod, 'd, 'r) Fold.t, 'k) Fold.s1

   val By : ('b,
             (('f, 't, 'x) mod, 'd, 'r) Fold.t,
             (('f, 't, 'b) mod, 'd, 'r) Fold.t, 'k) Fold.s1

   (** == Iterating over Integer Ranges == *)

   val upTo : Int.t -> (((Int.t, Int.t, Int.t) mod,
                         (Int.t, Int.t, Int.t) mod,
                         Int.t t) Fold.t, 'k) CPS.t
   (**
    *> upTo u From l By d $ =
    *>    [<l + 0*d, l + 1*d, ..., l + (u-l) div d * d>]
    *
    * Defaults: {From 0 By 1}
    *)

   val downFrom : Int.t -> (((Int.t, Int.t, Int.t) mod,
                             (Int.t, Int.t, Int.t) mod,
                             Int.t t) Fold.t, 'k) CPS.t
   (**
    *> downFrom u To l By d $ =
    *>    [<u - 1*d, u - 2*d, ..., u - (u-l+d-1) div d * d>]
    *
    * Note that {u - (u-l+d-1) div d * d} may be less than {l}.
    *
    * Defaults: {To 0 By 1}
    *)

   val up : (((Int.t, Unit.t, Int.t) mod,
              (Int.t, Unit.t, Int.t) mod,
              Int.t t) Fold.t, 'k) CPS.t
   (**
    *> up From l By d $ = [<l + 0*d, l + 1*d, ...>]
    *
    * Defaults: {From 0 By 1}
    *)

   val down : (((Int.t, Unit.t, Int.t) mod,
                (Int.t, Unit.t, Int.t) mod,
                Int.t t) Fold.t, 'k) CPS.t
   (**
    *> down From u By d $ = [<u - 1*d, u - 2*d, ...>]
    *
    * Defaults: {From 0 By 1}
    *)

   val integers : Int.t t
   (** {integers = up $ = [<0, 1, 2, ...>]} *)

   (** == Iterating over Non-Integer Ranges == *)

   val realsTo : Real.t -> (((Real.t, Unit.t, Real.t) mod,
                             (Real.t, Unit.t, Real.t) mod,
                             Real.t t) Fold.t, 'k) CPS.t
   (**
    *> realsTo b From a By s $ = [<a+0.0*s, a+1.0*s, ..., a+(n-1.0)*s>]
    *
    * where {n = (b-a)/s}.  If {n} is negative the sequence will be empty.
    * If {n} cannot be represented to sufficient precision or is NaN, then
    * {Domain} will be raised.
    *
    * Defaults: {From 0.0 By 1.0}
    *)

   (** == Indexing == *)

   val index : (((Int.t, Unit.t, Int.t) mod,
                 (Int.t, Unit.t, Int.t) mod,
                 'a t -> ('a, Int.t) Product.t t) Fold.t, 'k) CPS.t
   (**
    *> index From i By d $ [<x(0), x(1), ...>] =
    *>    [<x(0) & i+0*d, x(1) & i+1*d, ...>]
    *
    * Defaults: {From 0 By 1}
    *)

   (** == Iterators Over Standard Sequences ==
    *
    * Each of the {inX} iterators iterates over all the elements in the
    * given sequence of type {X.t}.
    *)

   val inList : 'a List.t -> 'a t
   val onList : 'a List.t -> 'a List.t t
   (**
    *> onList []                      = [<>]
    *> onList [x(0), x(1), ..., x(n)] =
    *>   [<[x(0), x(1), ..., x(n)],
    *>     [x(1), ..., x(n)],
    *>     ...,
    *>     [x(n)]>]
    *)

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

   (** == Iterators Over Input Streams == *)

   val lines : BasisTextIO.instream -> String.t Option.t
   (** This is the same as {TextIO.inputLine}. *)

   val chars : BasisTextIO.instream -> Char.t Option.t
   (** This is the same as {TextIO.input1}. *)

   val inTextFile :
       String.t ->
       (((Unit.t, Unit.t, BasisTextIO.instream -> Char.t Option.t) mod,
         (Unit.t, Unit.t, BasisTextIO.instream -> 'a Option.t) mod,
         'a t) Fold.t, 'k) CPS.t
   (**
    *> inTextFile file By method $
    *
    * Iterates over elements input from the text {file} by the given
    * {method}.
    *
    * Defaults: {By chars}
    *)

   val inDir : String.t -> String.t t
   (**
    * Iterates over the files in the specified directory.  This
    * corresponds to iterating over the files returned by repeatedly
    * calling {OS.FileSys.readDir} with a directory stream opened with
    * {OS.FileSys.openDir}.
    *)
end
