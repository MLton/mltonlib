(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Stream} module.
 *
 * The design and implementation is based on ideas from the following
 * article:
 *
 *   Stream Fusion: From Lists to Streams to Nothing at All.
 *   Duncan Coutts, Roman Leshchinskiy, and Don Stewart.
 *   Proceedings of the ACM SIGPLAN International Conference on Functional
 *   Programming, ICFP 2007.
 *   [http://www.cse.unsw.edu.au/~dons/papers/CLS07.html]
 *)
signature STREAM = sig
   type 'a t

   (** == Eliminating Streams == *)

   val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val app : 'a Effect.t -> 'a t Effect.t

   (** == Manipulating Streams == *)

   val map : ('a -> 'b) -> 'a t -> 'b t
   val filter : 'a UnPr.t -> 'a t UnOp.t

   (** == Introducing Streams == *)

   val tabulate : Int.t * (Int.t -> 'a) -> 'a t
   val unfoldr : ('s -> ('a * 's) Option.t) -> 's -> 'a t

   (** == Conversions == *)

   val fromArray  : 'a  Array.t ->     'a t
   val fromList   : 'a   List.t ->     'a t
   val fromString :    String.t -> Char.t t
   val fromVector : 'a Vector.t ->     'a t

   val fromArraySlice  :  'a ArraySlice.t ->     'a t
   val fromSubstring   :      Substring.t -> Char.t t
   val fromVectorSlice : 'a VectorSlice.t ->     'a t

   val toArray  :     'a t -> 'a  Array.t
   val toList   :     'a t -> 'a   List.t
   val toString : Char.t t ->    String.t
   val toVector :     'a t -> 'a Vector.t
end
