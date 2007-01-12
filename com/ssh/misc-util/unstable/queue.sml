(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of the {QUEUE} signature.  This is based on a space
 * safe implementation by Stephen Weeks posted on the MLton developers
 * mailing list.
 *)

structure Queue :> QUEUE = struct
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
end
