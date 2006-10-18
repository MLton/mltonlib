(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Utility module for dealing with isomorphisms.
 *)

signature ISO =
   sig
      type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)

      val id : ('a, 'a) iso

      val to : ('a, 'b) iso -> 'a -> 'b
      val from : ('a, 'b) iso -> 'b -> 'a
   end

structure Iso :> ISO =
   struct
      type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)

      val id = (fn a => a, fn a => a)

      fun to (a2b, _) = a2b
      fun from (_, b2a) = b2a
   end

type ('a, 'b) iso = ('a, 'b) Iso.iso
