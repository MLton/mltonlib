(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Void} module.
 *
 * The idea of the {Void} module is that there is no way to create values
 * of the {Void.t} type.  This means that a function whose range is
 * {Void.t} can not return normally.  Such a function can only either
 * raise an exception or call some other function that never returns.  On
 * the other hand, a function whose domain is {Void.t} can never be
 * called.
 *
 * Specifying {Void.t} as the range of a function that never returns may
 * perhaps communicate the semantics of the function most directly:
 *
 *> val neverReturns : d -> Void.t
 *
 * It may also help to work around the value restriction in some cases.
 * However, it is usually better to use a type variable as the range of
 * function that never returns:
 *
 *> val neverReturns : d -> 'a
 *
 * In SML, a function whose range is a type variable that does not occur
 * in the domain of the function can never return normally.  The benefit
 * of using such a specification is that the function can be called
 * conveniently from a context of any type.
 *
 * There is another more interesting use for {Void.t}.  Consider the
 * following specification:
 *
 *> val mystery : (d -> Void.t) -> r
 *
 * The specification ensures that the mystery function can only be called
 * with a function that never returns normally.  This can sometimes be a
 * useful property to ensure.
 *)
signature VOID = sig
   type t
   (** A type that has no values. *)

   val void : t -> 'a
   (**
    * This function can never be called, because there is no way to create
    * values of type {Void.t}.  However, you can use {void} to call a
    * function {f : d -> Void.t} from a context of any type by writing
    * {void (f x)}.
    *)
end
