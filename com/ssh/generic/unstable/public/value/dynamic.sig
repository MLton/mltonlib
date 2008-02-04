(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic, structural, dynamic type.
 *
 * The coercion functions {toDynamic} and {fromDynamic} take time relative
 * to the size of the structural encoding of the values.  Mutable types,
 * {ref}s and {array}s, are not supported---encoding would not preserve
 * the identity of mutable values.  Arrow types are supported, but
 * coercing a function to a dynamic value and then back returns a function
 * wrapped with coercions.
 *
 * In contrast to the universal type provided by the {Univ} structure, the
 * provided dynamic type is structural.  Consider the following code:
 *
 *> val x = toDynamic (list int) [5]
 *> val SOME [5] = fromDynamic (list int) x
 *
 * Even though the generic representation {list int} is computed twice,
 * the above code evaluates without raising a {Bind} exception.
 *
 * However, it is possible to have multiple different structural encodings
 * of a type.  Coercions between values of different structural encodings
 * may (or may not) fail.
 *
 * It is also possible to have multiple different types that have the same
 * structural encoding.  Such types cannot be told apart and coercions
 * between values of such types do not fail (by default).
 *
 * This design is experimental.  An interesting design alternative would
 * be to allow more coercions to occur in {fromDynamic}.  For example,
 * coercions between different scalar sizes and types could be performed
 * implicitly.  It would also be possible to coerce between vectors and
 * lists of different element type.  One could even implicitly read values
 * from strings.  It would also be possible to maximize structural sharing
 * during coercions.  Mutable types could be supported up to structural
 * isomorphism of the values.  It might also make sense to provide a
 * read-only view of the encoding.  That would allow clients to implement
 * various functions outside the dynamic module.  Alternatively, many
 * interesting primitives could be added, e.g. {apply : t -> t -> t}.
 * Feedback on the design is welcome!
 *
 * A dynamic type could also be implemented through pickling.  However,
 * functions cannot be pickled in SML and pickling of exceptions requires
 * registering exception constructors.
 *)
signature DYNAMIC = sig
   structure DynamicRep : OPEN_REP

   structure Dynamic : sig
      type t
      exception Dynamic
   end

   val toDynamic : ('a, 'x) DynamicRep.t -> 'a -> Dynamic.t
   val fromDynamic : ('a, 'x) DynamicRep.t -> Dynamic.t -> 'a Option.t
end

signature DYNAMIC_CASES = sig
   include CASES DYNAMIC
   sharing Open.Rep = DynamicRep
end

signature WITH_DYNAMIC_DOM = CASES
