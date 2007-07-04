(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Operations on sorted (or ordered) lists.  The provided signature is not
 * type safe meaning that it is possible to apply these operations to
 * unsorted lists as well as lists sorted with a different compare
 * function.
 *)

structure SortedList :> sig
   type 'a policy
   type 'a card = {1 : 'a policy, n : 'a policy} -> 'a policy
   (**
    * Cardinality policy is specified as either {#1} or {#n}.  {#1}
    * means that a sorted list has at most 1 element of any value,
    * while {#n} means that a list may have any number of equal values.
    *)

   val insert : 'a card -> 'a Cmp.t -> 'a -> 'a List.t UnOp.t
   (** {insert #? cmp x xs = merge #? cmp ([x], xs)} *)

   val isSorted : 'a card -> 'a Cmp.t -> 'a List.t UnPr.t
   (**
    * Returns true iff the list is sorted to the specified cardinality and
    * ordering.
    *)

   val merge : 'a card -> 'a Cmp.t -> 'a List.t BinOp.t
   (**
    * Merges two lists sorted to the specified cardinality and ordering.
    *
    * It is guaranteed that in {merge #n cmp (l, r)} elements from the
    * list {l} appear before equal elements from the list {r}.
    *)

   val remove : 'a card -> 'a Cmp.t -> 'a -> 'a List.t UnOp.t
   (**
    * Removes the specified cardinality of elements that compare equal to
    * the specified element from the sorted list.
    *)

   val stableSort : 'a card -> 'a Cmp.t -> 'a List.t UnOp.t
   (**
    * Sorts the given list to the specified cardinality and ordering.
    *
    * It is guaranteed that the relative ordering of equal elements is
    * retained.
    *)
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix <\ >|
   (* SML/NJ workaround --> *)

   type 'a policy = {cond : Order.t UnPr.t,
                     cont : 'a List.t Sq.t UnOp.t UnOp.t,
                     dups : 'a * 'a List.t -> 'a List.t}
   type 'a card = {1 : 'a policy, n : 'a policy} -> 'a policy

   fun P m (c : 'a card) =
       {1 = {cond = LESS <\ op =,
             cont = const id,
             dups = Pair.snd},
        n = {cond = GREATER <\ op <>,
             cont = id,
             dups = op ::}} >| c >| m

   fun isSorted card compare = let
      fun lp [] = true
        | lp [_] = true
        | lp (x1::(xs as x2::_)) =
          P #cond card (compare (x1, x2))
          andalso lp xs
   in
      lp
   end

   fun revMerge' #? compare (xs, ys) = let
      fun lp ([], ys, zs) = (ys, zs)
        | lp (xs, [], zs) = (xs, zs)
        | lp (x::xs, y::ys, zs) =
          case compare (x, y) of
             LESS => lp (xs, y::ys, x::zs)
           | EQUAL => lp (xs, P #dups #? (y, ys), x::zs)
           | GREATER => lp (x::xs, ys, y::zs)
   in
      lp (xs, ys, [])
   end

   fun merge #? ? = List.revAppend o Pair.swap o revMerge' #? ?

   fun insert #? compare x xs = merge #? compare ([x], xs)

   fun remove #? compare x ys = let
      fun lp (zs, []) = (zs, [])
        | lp (zs, y::ys) =
          case compare (x, y) of
             LESS => (zs, y::ys)
           | EQUAL => P #cont #? lp (zs, ys)
           | GREATER => lp (y::zs, ys)
   in
      List.revAppend (lp ([], ys))
   end

   (*
    * This is an optimized implementation of merge sort that tries to
    * avoid unnecessary list reversals.  This is done by performing
    * reverse merges and flipping the compare direction as appropriate.
    *)
   fun stableSort #? compare = let
      fun revOdd (w, l) = if Word.isEven w then l else rev l
      fun merge r =
          List.revAppend o (if Word.isOdd r then revMerge' #? compare
                            else revMerge' #? (compare o Pair.swap) o Pair.swap)
      val finish =
          fn [] => []
           | e::es =>
             revOdd
                (foldl
                    (fn ((r1, l1), (r0, l0)) =>
                        (r1+0w1, merge (r1+0w1) (revOdd (r1-r0, l0), l1)))
                    e es)
      fun build (stack as ((r0, l0)::(r1, l1)::rest)) =
          if r0 <> r1 then push stack
          else build ((r1+0w1, merge (r1+0w1) (l0, l1))::rest)
        | build stack = push stack
      and push stack =
          fn [] => finish stack
           | x::xs => let
             fun lp y ys =
                 fn [] => finish ((0w1, y::ys)::stack)
                  | x::xs =>
                    case compare (x, y) of
                       LESS => build ((0w1, y::ys)::stack) (x::xs)
                     | EQUAL => lp x (P #dups #? (y, ys)) xs
                     | GREATER => lp x (y::ys) xs
          in
             lp x [] xs
          end
   in
      push []
   end
end
