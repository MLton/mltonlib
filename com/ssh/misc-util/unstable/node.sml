(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Imperative singly linked list node.  This is useful and possibly more
 * convenient and efficient than a functional list when implementing
 * imperative data structures (e.g. imperative hast tables).
 *
 * Note that imperative lists may form cycles and, unless otherwise
 * specified, procedures specified in this module are not specifically
 * designed to work with cyclic lists.
 *)
structure Node :> sig
   type 'a t
   type 'a p = 'a t Option.t Ref.t

   val new : 'a -> 'a t
   val ptr : 'a p Thunk.t

   val next : 'a t -> 'a p
   val value : 'a t -> 'a

   val isEmpty : 'a p UnPr.t

   val length : 'a p -> Int.t

   val hd : 'a p -> 'a
   val tl : 'a p UnOp.t

   val push : 'a p -> 'a Effect.t
   val pop : 'a p -> 'a Option.t

   val peek : 'a p -> 'a Option.t

   val drop : 'a p Effect.t

   val find : 'a UnPr.t -> 'a p -> ('a p, 'a p) Sum.t
   val fold : ('a * 's -> 's) -> 's -> 'a p -> 's

   val toList : 'a p -> 'a List.t

   val filter : 'a UnPr.t -> 'a p UnOp.t

   val appClear : 'a Effect.t -> 'a p UnOp.t

   val insert : 'a BinPr.t -> 'a p -> 'a Effect.t
end = struct
   datatype 'a t = T of 'a * 'a p
   withtype 'a p = 'a t Option.t Ref.t

   fun ptr () = ref NONE
   fun new v = T (v, ptr ())

   fun next (T (_, p)) = p
   fun value (T (v, _)) = v

   fun isEmpty p = isNone (!p)

   fun nonEmpty f p = case !p of NONE => raise Empty | SOME n => f n
   fun hd p = nonEmpty value p
   fun tl p = nonEmpty next p

   fun drop p = p := !(tl p)

   fun push p v = let
      val n = new v
   in
      next n := !p ; p := SOME n
   end

   fun pop p =
       case !p of
          NONE => NONE
        | SOME (T (v, p')) => (p := !p' ; SOME v)

   fun peek p =
       case !p of
          NONE => NONE
        | SOME (T (v, _)) => SOME v

   fun find c p =
       case !p of
          NONE => INL p
        | SOME (T (v, p')) => if c v then INR p else find c p'

   fun fold f s p =
       case !p of
          NONE => s
        | SOME (T (v, p)) => fold f (f (v, s)) p

   fun toList p = rev (fold op :: [] p)

   fun length p = fold (1 <\ op + o #2) 0 p

   fun filter c p =
       case !p of
          NONE => p
        | SOME (T (v, n)) => if c v then filter c n else (p := !n ; filter c p)

   fun appClear ef p =
       case !p of
          NONE => p
        | SOME (T (v, n)) => (ef v : unit ; p := !n ; appClear ef p)

   fun insert lt p v =
       case !p of
          NONE => push p v
        | SOME (T (x, p')) =>
          if lt (x, v) then insert lt p' v else push p v
end
