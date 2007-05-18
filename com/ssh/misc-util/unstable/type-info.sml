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

   val canBeCyclic : 'a type_info_t UnPr.t
   (**
    * Returns true iff {'a} is of the form {'b ref} or {'b array} and
    * it can not be ruled out that values of the type can form cycles.
    *
    * Note: Functions are not considered to form cycles.
    *)

   val hasBaseCase : 'a type_info_t UnPr.t
   (** Returns true iff the type {'a} has a non-recursive variant. *)

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

   val numConsecutiveAlts : 'a type_info_t -> Int.t
   (**
    * Number of consecutive alternatives.
    *)
end

functor LiftTypeInfo
           (include TYPE_INFO
            type 'a t
            val lift : ('a type_info_t, 'a t) Lift.t Thunk.t) : TYPE_INFO = struct
   type 'a type_info_t = 'a t
   fun mk f = Lift.get lift f
   val canBeCyclic        = fn ? => mk canBeCyclic        ?
   val hasBaseCase        = fn ? => mk hasBaseCase        ?
   val hasExn             = fn ? => mk hasExn             ?
   val hasRecData         = fn ? => mk hasRecData         ?
   val isRefOrArray       = fn ? => mk isRefOrArray       ?
   val numConsecutiveAlts = fn ? => mk numConsecutiveAlts ?
end

structure TypeInfo :> sig
   include STRUCTURAL_TYPE TYPE_INFO
   sharing type type_info_t = t
end = struct
   datatype u =
      IN of {alts : Int.t,
             base : Bool.t,
             exn : Bool.t,
             pure : Bool.t,
             recs : Int.t List.t}
   fun out (IN t) = t
   type 'a t = u
   type 'a type_info_t = 'a t

   val hasBaseCase = #base o out
   val hasExn = #exn o out
   val hasRecData = not o null o #recs o out
   val isRefOrArray = not o #pure o out
   val numConsecutiveAlts = #alts o out
   val canBeCyclic = isRefOrArray andAlso (hasExn orElse hasRecData)

   val base = IN {alts = 1, base = true, exn = false, pure = true, recs = []}
   fun pure (IN {exn, recs, ...}) =
      IN {alts = 1, base = true, exn = exn, pure = true, recs = recs}

   fun iso (IN {base, exn, pure, recs, ...}) =
       const (IN {alts = 1, base = base, exn = exn, pure = pure, recs = recs})

   fun (IN {base = bl, exn = hl, recs = rl, ...}) *`
       (IN {base = br, exn = hr, recs = rr, ...}) =
       IN {alts = 1, base = bl andalso br, exn = hl orelse hr, pure = true,
           recs = SortedList.merge#1 Int.compare (rl, rr)}

   fun (IN {alts = al, base = bl, exn = hl, recs = rl, ...}) +`
       (IN {alts = ar, base = br, exn = hr, recs = rr, ...}) =
       IN {alts = al + ar, base = bl orelse br, exn = hl orelse hr, pure = true,
           recs = SortedList.merge#1 Int.compare (rl, rr)}

   val unit = base

   local
      val id = ref 0
   in
      fun Y ? =
          Tie.pure
             (fn () => let
                 val this = !id before id += 1
              in
                 (IN {alts = 1, base = false, exn = false, pure = true, recs = [this]},
                  fn IN {alts, base, exn, pure, recs} =>
                     IN {alts = alts, base = base, exn = exn, pure = pure,
                         recs = SortedList.remove #1 Int.compare this recs})
              end) ?
   end

   fun _ --> _ = base

   val exn = IN {alts = 1, base = true, exn = true, pure = true, recs = []}
   fun regExn _ _ = ()

   fun array (IN {exn, recs, ...}) =
       IN {alts = 1, base = true, exn = exn, pure = false, recs = recs}
   fun refc (IN {base, exn, recs, ...}) =
       IN {alts = 1, base = base, exn = exn, pure = false, recs = recs}

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
