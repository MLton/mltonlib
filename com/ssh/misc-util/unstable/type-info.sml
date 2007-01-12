(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of a type-indexed function for tracking a number of
 * important type properties.
 *
 * These type properties can be useful for both optimizations and for
 * ensuring correctness.  As an optimization one could, for example,
 * determine whether one needs to handle cyclic values (which can be
 * costly) or not.  As a correctness issue, one can avoid generating
 * infinite data structures or avoid performing non-terminating operations
 * on infinite data structures.
 *
 * This type-indexed function is unlikely to be directly useful in
 * application programs and is more likely to be used internally in the
 * implementation of some other type-indexed functions (e.g. pickling).
 *)

signature TYPE_INFO = sig
   type 'a type_info_t

   val hasExn : 'a type_info_t UnPr.t
   (** Returns true iff the type {'a} contains the type {exn}. *)

   val hasRecData : 'a type_info_t UnPr.t
   (**
    * Returns true iff the type {'a} contains recursive references to
    * datatypes.
    *)

   val isRefOrArray : 'a type_info_t UnPr.t
   (**
    * Returns true iff the type {'a} is of the form {'b array} or of
    * the form {'b ref}.
    *)

   val canBeCyclic : 'a type_info_t UnPr.t
   (**
    * Returns true iff {'a} is of the form {'b ref} or {'b array} and
    * it can not be ruled out that values of the type can form cycles.
    *
    * Note: Functions are not considered to form cycles.
    *)
end

functor LiftTypeInfo
           (include TYPE_INFO
            type 'a t
            val lift : ('a type_info_t, 'a t) Lift.t Thunk.t) : TYPE_INFO = struct
   type 'a type_info_t = 'a t
   val hasExn       = fn ? => Lift.get lift hasExn       ?
   val hasRecData   = fn ? => Lift.get lift hasRecData   ?
   val isRefOrArray = fn ? => Lift.get lift isRefOrArray ?
   val canBeCyclic  = fn ? => Lift.get lift canBeCyclic  ?
end

structure TypeInfo :> sig
   include STRUCTURAL_TYPE
   include TYPE_INFO where type 'a type_info_t = 'a t
end = struct
   datatype u = IN of {exn : Bool.t, pure : Bool.t, recs : Int.t List.t}
   fun out (IN t) = t
   type 'a t = u
   type 'a type_info_t = 'a t

   val hasExn = #exn o out
   val hasRecData = not o null o #recs o out
   val isRefOrArray = not o #pure o out
   val canBeCyclic = isRefOrArray andAlso (hasExn orElse hasRecData)

   val base = IN {exn = false, pure = true, recs = []}
   fun pure (IN {exn, recs, ...}) = IN {exn = exn, pure = true, recs = recs}
   fun impure (IN {exn, recs, ...}) =
       IN {exn = exn, pure = false, recs = recs}
   fun combine (IN {exn = hl, recs = rl, ...},
                IN {exn = hr, recs = rr, ...}) =
       IN {exn = hl orelse hr, pure = true,
           recs = SortedList.merge#1 Int.compare (rl, rr)}

   val iso = const

   val op *` = combine
   val op +` = combine

   val unit = base

   local
      val id = ref 0
   in
      fun Y ? =
          Tie.pure
             (fn () => let
                 val this = !id before id += 1
              in
                 (IN {exn = false, pure = true, recs = [this]},
                  fn IN {exn, pure, recs} =>
                     IN {exn = exn, pure = pure,
                         recs = SortedList.remove
                                   #1 Int.compare this recs})
              end) ?
   end

   fun _ --> _ = base

   val exn = IN {exn = true, pure = true, recs = []}
   fun regExn _ _ = ()

   val array = impure
   val refc  = impure

   val vector = pure

   val largeInt  = base
   val largeReal = base
   val largeWord = base

   val list = pure

   val bool   = base
   val char   = base
   val int    = base
   val real   = base
   val string = base
   val word   = base

   val word8  = base
   val word16 = base
   val word32 = base
   val word64 = base
end
