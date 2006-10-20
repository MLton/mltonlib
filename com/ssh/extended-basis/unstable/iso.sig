(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)
(**
 * Isomorphism between {'a} and {'b} with injection and projection
 * functions.
 *)

(**
 * Signature for the {Iso} structure for isomorphisms.
 *)
signature ISO = sig
   type ('a, 'b) iso = ('a, 'b) iso

   val id : ('a, 'a) iso

   val to : ('a, 'b) iso -> 'a -> 'b
   val from : ('a, 'b) iso -> 'b -> 'a
end
