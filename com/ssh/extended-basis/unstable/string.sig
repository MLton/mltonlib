(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {STRING} signature.
 *)

signature STRING =
   sig
      include STRING
      val listIso : (string, char list) iso
      val cStringEmb : (string, string) emb
      val stringEmb : (string, string) emb

      type vector = string
      type elem = char

      val all : (elem -> bool) -> vector -> bool
      val app  : (elem -> unit) -> vector -> unit
      val appi : (int * elem -> unit) -> vector -> unit
      val exists : (elem -> bool) -> vector -> bool
      val find  : (elem -> bool) -> vector -> elem option
      val findi : (int * elem -> bool) -> vector -> (int * elem) option
      val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
      val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
      val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
      val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
      val fromList : elem list -> vector
      val length : vector -> int
      val mapi : (int * elem -> elem) -> vector -> vector
      val maxLen : int
      val tabulate : int * (int -> elem) -> vector
      val toList : vector -> elem list
      val update : vector * int * elem -> vector
   end
