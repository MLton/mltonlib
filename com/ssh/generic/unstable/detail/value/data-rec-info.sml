(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDataRecInfo (Arg : WITH_DATA_REC_INFO_DOM) : DATA_REC_INFO_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  2 andAlso
   infix  1 orElse
   (* SML/NJ workaround --> *)

   structure Set :> sig
      type 'a t
      val empty : 'a t
      val isEmpty : 'a t UnPr.t
      val singleton : 'a -> 'a t
      val union : 'a t BinOp.t
      val remIf : 'a UnPr.t -> 'a t UnOp.t
   end = struct
      type 'a t = 'a UnPr.t UnPr.t
      fun empty _ = true
      fun isEmpty isEmpty = isEmpty (fn _ => false)
      fun singleton x rem = rem x
      fun union (isEmptyL, isEmptyR) =
       fn rem => isEmptyL rem andalso isEmptyR rem
      fun remIf p isEmpty rem = isEmpty (fn x => p x orelse rem x)
   end

   type recs = Exn.t Set.t

   datatype t = INT of {exn : Bool.t, recs : recs, pure : Bool.t}
   datatype s = INS of {exn : Bool.t, recs : recs}
   datatype p = INP of {exn : Bool.t, recs : recs}

   val base = INT {exn = false, pure = true, recs = Set.empty}
   fun pure (INT {exn, recs, ...}) = INT {exn = exn, pure = true, recs = recs}
   fun mutable (INT {exn, recs, ...}) =
       INT {exn = exn, pure = false, recs = recs}

   structure DataRecInfoRep = LayerRep
     (open Arg
      type 'a t = t and 'a s = s and ('a, 'k) p = p)

   open DataRecInfoRep.This

   fun outT (INT r) = r

   fun mayContainExn ? = (#exn o outT o getT) ?
   fun mayBeRecData  ? = (not o Set.isEmpty o #recs o outT o getT) ?
   fun isMutableType ? = (not o #pure o outT o getT) ?
   fun mayBeCyclic   ? =
       (isMutableType andAlso (mayContainExn orElse mayBeRecData)) ?

   structure Open = LayerCases
     (val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` (INP l, INP r) =
          INP {exn = #exn l orelse #exn r, recs = Set.union (#recs l, #recs r)}
      fun T (INT {exn, recs, ...}) = INP {exn = exn, recs = recs}
      fun R _ = T
      fun tuple (INP {exn, recs, ...}) =
          INT {exn = exn, pure = true, recs = recs}
      val record = tuple

      fun op +` (INS l, INS r) =
          INS {exn = #exn l orelse #exn r, recs = Set.union (#recs l, #recs r)}
      val unit = base
      fun C0 _ = INS {exn = false, recs = Set.empty}
      fun C1 _ (INT {exn, recs, ...}) = INS {exn = exn, recs = recs}
      fun data (INS {exn, recs, ...}) =
          INT {exn = exn, pure = true, recs = recs}

      val Y = Tie.pure (fn () => let
         exception Me
      in
         (INT {exn = false, pure = true, recs = Set.singleton Me},
          fn INT {exn, pure, recs} =>
             INT {exn = exn, pure = pure,
                  recs = Set.remIf (fn Me => true | _ => false) recs})
      end)

      fun op --> _ = base

      val exn = INT {exn = true, pure = true, recs = Set.empty}
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      val array = mutable
      val refc  = mutable

      val vector = pure
      val list   = pure

      val fixedInt = base
      val largeInt = base

      val largeReal = base
      val largeWord = base

      val bool   = base
      val char   = base
      val int    = base
      val real   = base
      val string = base
      val word   = base

      val word8  = base
      val word32 = base
(*
      val word64 = base
*)

      fun hole () = base

      open Arg DataRecInfoRep)
end
