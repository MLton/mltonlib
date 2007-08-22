(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for an experimental, deterministic, unified IO stream/state
 * monad base module {IOSMonad}.
 *
 * == About the Design ==
 *
 * The goal of this module is to provide a unified basis for
 * deterministic, monadic IO action combinators.  By deterministic it is
 * meant that the design does not explicitly support non-deterministic IO
 * such as backtracking parser combinators.  By unified it is meant that
 * the design is supposed to support both input and output actions equally
 * well.  One reason behind using monadic combinators is the implicit
 * threading of state, which is often convenient.
 *
 * The Basis Library provides the {('answer, 'state) StringCvt.reader}
 * type
 *
 *> type ('answer, 'state) reader = 'state -> ('answer * 'state) Option.t
 *
 * for functional stream readers.  It can be augmented to a monad by
 * providing implementations of {return} and {>>=}:
 *
 *> val return : 'a -> ('a, 's) reader =
 *>  fn a => fn s => SOME (a, s)
 *
 *> val op >>= : ('a, 's) reader * ('a -> ('b, 's) reader) -> ('b, 's) reader =
 *>  fn (aM, a2bM) =>
 *>     fn s =>
 *>        case aM s
 *>         of NONE        => NONE
 *>          | SOME (a, s) => a2bM a s
 *
 * While the reader type is technically flexible enough for almost
 * anything, there are some warts.
 *
 * Returning an option gives special support for a single special
 * condition --- typically an end-of-stream condition.  Other special
 * conditions or errors still need to be handled in some other way.  For
 * example, scanning functions, such as {Int.scan}, raise exceptions under
 * other error conditions.  A wart with returning an option is that
 * whether or not an end-of-stream condition is a special condition to be
 * expected or an unexpected error depends on the situation.  Many parsers
 * are deterministic in the sense that they know when to stop reading the
 * input.  If the input ends prematurely, it is a non-recoverable error.
 *
 * The Basis Library does not provide a type for functional writers.
 * Handling writers through the {reader} type would be possible.  A writer
 * would have a type of the form {'a -> (Unit.t, 's) reader}.  IOW, a
 * writer is a function from values to readers (or IO actions).  This
 * would allow unified handling of writers (and readers) using the same
 * monadic combinators.  However, there is no single special condition
 * that would nicely translate to returning {NONE}.  In other words, for
 * writers, returning an option is an unnecessary hurdle.  All writers
 * would just raise exceptions on error conditions.
 *
 * In a purely functional language, that does not provide core language
 * support for exceptions, it would be natural for IO actions to return
 * either an error value or an answer and the resulting state.  While such
 * a type would work in Standard ML, which does provide exceptions, it
 * would not rule out the possibility of raising exceptions in IO actions.
 * IO combinators would have to deal with two kinds of errors.  Therefore
 * it seems that handling special conditions or errors solely through core
 * language exceptions would be simpler, both conceptually and
 * implementation wise.
 *
 * Thus we arrive at the traditional IO monad approximation or the state
 * monad defined as follows:
 *
 *> type ('answer, 'state) t = 'state -> 'answer * 'state
 *
 *> val return : 'a -> ('a, 's) t =
 *>  fn a => fn s => (a, s)
 *
 *> val op >>= : ('a, 's) t * ('a -> ('b, 's) t) -> ('b, 's) t =
 *>  fn (aM, a2bM) =>
 *>     fn s => let
 *>        val (a, s) = aM s
 *>     in
 *>        a2bM a s
 *>     end
 *
 * The question is then how do core language exceptions interact with the
 * above monad.
 *
 * It turns out that adding core language exceptions to the above monad
 * essentially gives us a monad with zero(es).  A primitive zero could
 * be implemented as
 *
 *> val zero : ('a, 's) t =
 *>  fn _ => raise exn
 *
 * where {exn} is some exception.  It is easy to verify that {zero} is a
 * left zero for {>>=}:
 *
 *> zero >>= k = zero
 *
 * Interestingly, with core language exceptions, it is not necessary to
 * introduce a primitive zero.  Zeroes can be introduced with the
 * following combinator:
 *
 *> val fail : Exn.t -> ('a, 's) t =
 *>  fn exn = return () >>= raising exn
 *
 * It is further possible to extend from monad with zero to a monad with
 * plus by adding a primitive alternation combinator:
 *
 *> val op <|> : ('a, 's) t BinOp.t =
 *>  fn (lM, rM) =>
 *>     fn s =>
 *>        lM s handle _ => rM s
 *
 * It is easy to verify that {<|>} and {zero} form a monoid:
 *
 *> zero <|> a = a
 *> a <|> zero = a
 *> (a <|> b) <|> c = a <|> (b <|> c)
 *
 * The "Left Catch" law also holds trivially:
 *
 *> return a <|> b = return a
 *
 * The "Left Distribution" law
 *
 *> (a <|> b) >>= k = (a >>= k) <|> (b >>= k)
 *
 * does not hold, however.  If {a <|> b} succeeds with {a}, then {b} will
 * not be tried.
 *
 * Unfortunately, the alternation combinator is less useful than it might
 * first appear, because it typically leads to space leaks by holding on
 * to the entire input or state {s} until the computation finishes.  To
 * fix space-leaks would require a considerably more complicated monad and
 * the design space is much larger.
 *
 * == Idioms ==
 *
 * The reader type of the Basis Library treats the end-of-stream condition
 * as a special case.  Parsers that do not necessarily fail at
 * end-of-stream, can be written in several ways.  Perhaps the most
 * obvious way is to use an input operation that returns an option:
 *
 *> val read : (Char.t Option.t, 's) IOSMonad.t -> ('a, 's) IOSMonad.t
 *
 * Another way is to parameterize the parser with both an end-of-stream
 * predicate {eos} and an input operation {input} as follows:
 *
 *> val read : {eos : (Bool.t, 's) IOSMonad.t,
 *>             input : (Char.t, 's) IOSMonad.t} -> ('a, 's) IOSMonad.t
 *
 * It is also possible to convert between the two.
 *
 * When using monads in a strict language, it is sometimes necessary to
 * introduce additional laziness to avoid too eager evaluation.  Consider
 * the following function for creating an IO action that writes a list of
 * values:
 *
 *> fun writeList []      = return ()
 *>   | writeList (x::xs) = write x >> writeList xs
 *
 * The problem is that applying {writeList aList} immediately loops over
 * the entire list without performing any actions.  This is typically not
 * desired.  Fortunately, adding the desired laziness is easy.  One can
 * just use {>>=} as follows:
 *
 *> fun writeList []      = return ()
 *>   | writeList (x::xs) = write x >>= (fn () => writeList xs)
 *
 * Now, applying {writeList aList} merely examines the first cons of the
 * list.
 *)
signature IOS_MONAD = sig
   type ('answer, 'state) t = 'state -> 'answer * 'state
   (**
    * The IO / State Monad base type.  In each particular instance of the
    * monad the {'state} type parameter is fixed.
    *)

   (** == Monad Base Operations == *)

   val return : 'a -> ('a, 's) t
   val >>= : ('a, 's) t * ('a -> ('b, 's) t) -> ('b, 's) t

   (** == Lifting == *)

   exception EOS
   (** Exception for signaling end-of-stream. *)

   val fromReader : ('a, 's) Reader.t -> ('a, 's) t
   (**
    * Lifts a reader to the monad.  Unlike a reader, the returned action
    * raises {EOS} at end-of-stream.
    *)

   val fromWriter : ('a, 's) Writer.t -> 'a -> (Unit.t, 's) t
   (** Lifts a functional writer to the monad. *)

   val fromPutter : ('s * 'a) Effect.t -> 'a -> (Unit.t, 's) t
   (** Lifts an imperative put effect to the monad. *)

   (** == Additional Combinators == *)

   val mapState : ('s, 't) Iso.t -> ('a, 't) t -> ('a, 's) t
   (** Functionally updates the state/stream of the given action. *)

   val map : ('a -> 'b) -> ('a, 's) t -> ('b, 's) t
   (** {map a2b aT} is equivalent to {aT >>= return o a2b}. *)
end
