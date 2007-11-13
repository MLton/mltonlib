(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnlinkableList :> UNLINKABLE_LIST = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   type 'a p = 'a Option.t Ref.t
   val ! = fn p => case !p of SOME it => it | _ => fail "bug"
   val op := = fn (r, v) => r := SOME v

   datatype 'a t = RING of 'a l | NODE of {link : 'a l, value : 'a}
   withtype 'a l = {pred : 'a t p, succ : 'a t p}

   val link = fn RING link => link | NODE {link, ...} => link
   fun pred n = #pred (link n)
   fun succ n = #succ (link n)

   fun newLink () = {pred = ref NONE, succ = ref NONE}

   fun new () = let
      val l = newLink () val r = RING l
   in
      #pred l := r ; #succ l := r ; r
   end

   fun mkUnlink {pred = lp, succ = ls} () = let
      val p = !lp val s = !ls val n = ! (succ p)
   in
      ls := n ; lp := n ; succ p := s ; pred s := p
   end

   fun push (p, s, v) = let
      val l = newLink () val n = NODE {link = l, value = v}
   in
      #pred l := p ; #succ l := s ; pred s := n ; succ p := n
    ; mkUnlink l
   end
   fun pushFront r v = push (r, ! (succ r), v)
   fun pushBack r v = push (! (pred r), r, v)

   fun pop which r =
       case ! (which r) of
          RING _ => NONE
        | NODE {link, value} => (mkUnlink link () ; SOME value)
   fun popFront r = pop succ r
   fun popBack r = pop pred r
end
