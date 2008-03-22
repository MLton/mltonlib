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
structure LinkedQueue :> LINKED_QUEUE = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   datatype 'a t = T of {back : 'a Node.p Ref.t, front : 'a Node.p Ref.t}

   fun new () =
       case Node.ptr ()
        of p => T {back = ref p, front = ref p}

   fun isEmpty (T {front, ...}) =
       Node.isEmpty (!front)

   fun length (T {front, ...}) =
       Node.length (!front)

   fun enque (T {back, ...}) =
    fn v => let
          val r = !back
          val n = Node.new v
       in
          r := SOME n
        ; back := Node.next n
       end

   fun deque (T {front, ...}) =
       case !(!front)
        of NONE   => NONE
         | SOME n => (front := Node.next n ; SOME (Node.value n))

   fun filter c (T {back, front}) =
       back := Node.filter c (!front)

   fun filterOut c = filter (neg c)

   fun appClear ef (T {back, front}) =
       back := Node.appClear ef (!front)
end
