(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Extended {LIST} signature. *)
signature LIST = sig
   include LIST

   type 'a t = 'a list
   (** Convenience alias. *)

   (** == Basic == *)

   val sub : 'a t * Int.t -> 'a
   (**
    * {sub (l, i)} returns the {i}th element of the list {l}.  This is
    * equivalent to {nth}.
    *)

   val init : 'a t UnOp.t
   (**
    * Return all the elements of a list except the last one.  Raises
    * {Empty} if the list is empty.
    *)

   (** == Transformations == *)

   val intersperse : 'a -> 'a t UnOp.t
   (**
    * {intersperse d l} forms the list {[sub (l, 0), d, sub (l, 1), d,
    * ..., d, sub (l, n-1)]} where {n = length l}.
    *)

   val transpose : 'a t t UnOp.t
   (** Transposes the rows and columns of its argument. *)

   val index : 'a t -> (Int.t * 'a) t
   (**
    * {index l} returns the list {[(0, sub (l, 0)), (1, sub (l, 1)), ...,
    * (n-1, sub (l, n-1))]} where {n = length l} is the length of the
    * given list.
    *)

   (** == Stack == *)

   val push : ('a t Ref.t * 'a) Effect.t
   val pop : 'a t Ref.t -> 'a Option.t

   (** == HOFs == *)

   val foldl1 : 'a BinOp.t -> 'a t -> 'a
   val foldr1 : 'a BinOp.t -> 'a t -> 'a

   val appr : 'a Effect.t -> 'a t Effect.t
   (** {appr f l} applies {f} to the elements of {l}, from right to left. *)

   val concatMap : ('a -> 'b t) -> 'a t -> 'b t

   (** == Indexed HOFs == *)

   val appi : (Int.t * 'a) Effect.t -> 'a t Effect.t
   val appri : (Int.t * 'a) Effect.t -> 'a t Effect.t
   val concatMapi : (Int.t * 'a -> 'b t) -> 'a t -> 'b t
   val mapi : (Int.t * 'a -> 'b) -> 'a t -> 'b t
   val mapiPartial : (Int.t * 'a -> 'b Option.t) -> 'a t -> 'b t
   val foldli : (Int.t * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldri : (Int.t * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val alli : (Int.t * 'a) UnPr.t -> 'a t UnPr.t
   val existsi : (Int.t * 'a) UnPr.t -> 'a t UnPr.t
   val findi : (Int.t * 'a) UnPr.t -> 'a t -> (Int.t * 'a) Option.t

   (** == Special Folds == *)

   val maximum : 'a Cmp.t -> 'a t -> 'a
   val minimum : 'a Cmp.t -> 'a t -> 'a

   (** == Unfolding == *)

   val unfoldl : ('a -> ('b * 'a) Option.t) -> 'a -> 'b list
   val unfoldl' : ('a -> ('b * 'a) Option.t) -> 'a -> 'b list * 'a
   val unfoldr : ('a -> ('b * 'a) Option.t) -> 'a -> 'b list
   val unfoldr' : ('a -> ('b * 'a) Option.t) -> 'a -> 'b list * 'a

   (** == Extracting sublists == *)

   val split : 'a t * Int.t -> 'a t Sq.t
   (**
    * {split (l, i)} returns a pair f the first {i} and last {length l -
    * i} elements of the list {l}.  Raises {Subscript} if {i < 0 orelse
    * length l < i}.  Specifically, {split (l, n) = (take (l, n), drop (l,
    * n))}.
    *)

   val takeWhile : 'a UnPr.t -> 'a t UnOp.t
   (**
    * {takeWhile p xs} returns the longest prefix of {xs} of elements that
    * satisfy {p}.
    *)

   val dropWhile : 'a UnPr.t -> 'a t UnOp.t
   (** {dropWhile p xs} returns the suffix remaining after {takeWhile p xs}. *)

   val span : 'a UnPr.t -> 'a t -> 'a t Sq.t
   (** {span p xs = (takeWhile p xs, dropWhile p xs)}. *)

   (** == Set Operations == *)

   val contains : ''a t -> ''a UnPr.t
   (** {contains l x = exists (x <\ op =) l} *)

   (** == Equality == *)

   val equal : 'a BinPr.t -> 'a t BinPr.t
   (**
    * Given an equality predicate on an element type returns an equality
    * predicate on lists of the element type.
    *)
end
