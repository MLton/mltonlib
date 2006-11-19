(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {LIST} signature.
 *)
signature LIST = sig
   include LIST

   type 'a t = 'a list
   (** Convenience alias. *)

   (** == Basic == *)

   val sub : 'a t * int -> 'a
   (**
    * {sub (l, i)} returns the {i}th element of the list {l}.  This is
    * equivalent to {nth}.
    *)

   val init : 'a t -> 'a t
   (**
    * Return all the elements of a list except the last one.  Raises
    * {Empty} if the list is empty.
    *)

   val split : 'a t * int -> 'a t * 'a t
   (**
    * {split (l, i)} returns a pair f the first {i} and last {length l -
    * i} elements of the list {l}.  Raises {Subscript} if {i < 0 orelse
    * length l < i}.  Specifically, {split (l, n) = (take (l, n), drop (l,
    * n))}.
    *)

   (** == Transformations == *)

   val intersperse : 'a -> 'a t -> 'a t
   (**
    * {intersperse d l} forms the list {[sub (l, 0), d, sub (l, 1), d,
    * ..., d, sub (l, n-1)]} where {n = length l}.
    *)

   val transpose : 'a t t -> 'a t t
   (** Transposes the rows and columns of its argument. *)

   val index : 'a t -> (int * 'a) t
   (**
    * {index l} returns the list {[(0, sub (l, 0)), (1, sub (l, 1)), ...,
    * (n-1, sub (l, n-1))]} where {n = length l} is the length of the
    * given list.
    *)

   (** == Stack == *)

   val push : 'a t ref * 'a -> unit
   val pop : 'a t ref -> 'a option

   (** == HOFs == *)

   val foldl1 : ('a * 'a -> 'a) -> 'a t -> 'a
   val foldr1 : ('a * 'a -> 'a) -> 'a t -> 'a

   val appr : ('a -> unit) -> 'a t -> unit
   (** {appr f l} applies {f} to the elements of {l}, from right to left. *)

   val concatMap : ('a -> 'b t) -> 'a t -> 'b t

   (** == Indexed HOFs == *)

   val appi : (int * 'a -> unit) -> 'a t -> unit
   val appri : (int * 'a -> unit) -> 'a t -> unit
   val concatMapi : (int * 'a -> 'b t) -> 'a t -> 'b t
   val mapi : (int * 'a -> 'b) -> 'a t -> 'b t
   val mapiPartial : (int * 'a -> 'b option) -> 'a t -> 'b t
   val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val alli : (int * 'a -> bool) -> 'a t -> bool
   val existsi : (int * 'a -> bool) -> 'a t -> bool
   val findi : (int * 'a -> bool) -> 'a t -> (int * 'a) option

   (** == Set Operations == *)

   val contains : ''a t -> ''a -> bool

   (** == Equality == *)

   val equal : ('a * 'b -> bool) -> 'a t * 'b t -> bool
end
