(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of an extended version of the {QUEUE} signature.  The
 * extensions aren't part of the {QUEUE} signature, because they don't
 * make sense for all possible implementations of the signature.  This
 * implementation is based on a space safe implementation by Stephen Weeks
 * posted on the MLton developers mailing list.
 *)
structure Queue :> sig
   include QUEUE
   val filter : 'a UnPr.t -> 'a t Effect.t
   val filterOut : 'a UnPr.t -> 'a t Effect.t
   val foldClear : ('a * 's -> 's) -> 's -> 'a t -> 's
   val appClear : 'a Effect.t -> 'a t Effect.t
end = struct
   structure N = Node

   datatype 'a t = IN of {back : 'a N.t Ref.t,
                          front : 'a N.t Ref.t}

   fun new () = let
      val n = N.new ()
   in
      IN {back = ref n, front = ref n}
   end

   fun isEmpty (IN {front, ...}) =
       not (isSome (N.get (!front)))

   fun length (IN {front, ...}) =
       N.length (!front)

   fun enque (IN {back, ...}) =
       fn a => let
          val r = !back
          val n = N.new ()
       in
          N.<- (r, SOME (a, n))
        ; back := n
       end

   fun deque (IN {front, ...}) =
       case N.get (!front) of
          NONE => NONE
        | SOME (a, n) => (front := n ; SOME a)

   fun filter p (IN {back, front}) =
       back := Node.filter p (!front)

   fun filterOut p =
       filter (negate p)

   fun foldClear f s q =
       case deque q of
          NONE => s
        | SOME v => foldClear f (f (v, s)) q

   fun appClear ef = foldClear (ef o #1) ()
end
