(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor JoinLattice (S: JOIN_LATTICE_STRUCTS): JOIN_LATTICE = 
struct

open S

datatype t = T of {elt: Element.t ref,
                   lessThan: t list ref}

fun value (T {elt, ...}) = !elt

fun layout (T {elt, ...}) = Element.layout (!elt)

fun new e = T {elt = ref e,
               lessThan = ref []}

fun ensureAtLeast (T {elt, lessThan}, lowerBound) =
   if Element.<= (lowerBound, !elt)
      then ()
   else
      let
         val j = Element.join (lowerBound, !elt)
         val () = elt := j
      in
         List.foreach (!lessThan, fn e => ensureAtLeast (e, j))
      end

fun (T {elt, lessThan, ...}) <= to =
   (List.push (lessThan, to)
    ; ensureAtLeast (to, !elt))
                    
end
