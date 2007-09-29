(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for a generic size function.
 *)
signature SIZE = sig
   structure SizeRep : OPEN_REP

   val staticSizeOf : ('a, 'x) SizeRep.t -> Int.t Option.t
   (**
    * Returns an abstract, statically estimated, size of values of the
    * type {'a} in bytes.
    *
    * The sizes of functions (closures), sequences, arbitrary precision
    * integers, non-trivial sums, exceptions, and recursive datatypes
    * cannot be estimated statically.
    *)

   val sizeOf : ('a, 'x) SizeRep.t -> 'a -> Int.t
   (**
    * Returns an abstractly computed size of the given value in bytes.
    *
    * The size of a function (closure) cannot be computed in Standard ML.
    * An attempt to compute the size of a function will fail at run-time.
    *)
end

signature SIZE_CASES = sig
   include CASES SIZE
   sharing Open.Rep = SizeRep
end

signature WITH_SIZE_DOM = sig
   include CASES HASH TYPE_INFO
   sharing Open.Rep = HashRep = TypeInfoRep
end
