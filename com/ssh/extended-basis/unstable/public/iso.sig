(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Signature for the {Iso} structure for isomorphisms.
 *)
signature ISO = sig
   type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)
   (**
    * Isomorphism between {'a} and {'b} with injection and projection
    * functions.
    *)

   type ('a, 'b) t = ('a, 'b) iso
   (**
    * Convenience alias.
    *)

   val id : ('a, 'a) iso
   (**
    * The identity isomorphism.  This is always equivalent to {(fn a => a,
    * fn a => a)}.
    *)

   val to : ('a, 'b) iso -> 'a -> 'b
   (**
    * Extracts the injection part of the given isomorphism.
    *)

   val from : ('a, 'b) iso -> 'b -> 'a
   (**
    * Extracts the projection part of the given isomorphism.
    *)
end
