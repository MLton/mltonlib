(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for a generic dummy value.  In Standard ML, dummy values
 * are needed for things such as computing fixpoints and building cyclic
 * values.
 *
 * This generic is unlikely to be directly useful in application programs
 * and is more likely to be used internally in the implementation of some
 * other generics (e.g. pickling).
 *)
signature SOME = sig
   structure SomeRep : OPEN_REP

   exception Nothing of Exn.t
   (** Raised when trying to extract some value when there is none. *)

   val some : ('a, 'x) SomeRep.t -> 'a
   (** Returns some value of type {'a} or raises {Nothing}. *)

   val withNone : ('a, 'x) SomeRep.t UnOp.t
   (** Removes any value from the given representation. *)

   val withSome : 'a -> ('a, 'x) SomeRep.t UnOp.t
   (** Sets the value of the given representation. *)
end

signature SOME_CASES = sig
   include CASES SOME
   sharing Open.Rep = SomeRep
end

signature WITH_SOME_DOM = TYPE_INFO_CASES
