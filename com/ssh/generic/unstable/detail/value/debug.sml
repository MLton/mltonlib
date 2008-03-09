(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDebug (Arg : CASES) : OPEN_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   (* XXX Consider an asymptotically more efficient set representation. *)

   fun add1 kind (x, xs) =
       if List.exists (eq x) xs
       then fails ["Duplicate ", kind, "s: ", x]
       else x::xs

   fun addN kind (xs, ys) = foldl (add1 kind) xs ys

   local
      fun mk p k toString x =
          case toString x
           of s => if p s then s else fails ["Not a ", k, ": ", s]
   in
      val con   = mk SmlSyntax.isLongId "constructor" Generics.Con.toString
      val label = mk SmlSyntax.isLabel  "label"     Generics.Label.toString
   end

   val exns : String.t List.t Ref.t = ref []
   fun regExn c = exns := add1 "exception constructor" (con c, !exns)

   structure DebugRep = LayerRep
     (open Arg
      type  'a      t = Unit.t
      type  'a      s = String.t List.t
      type ('a, 'k) p = String.t List.t)

   structure Layered = LayerCases
     (val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` ? = addN "label" ?
      fun T () = []
      fun R l () = [label l]
      val tuple  = ignore
      val record = ignore

      fun op +` ? = addN "constructor" ?
      val unit = ()
      fun C0 c = [con c]
      fun C1 c () = [con c]
      val data = ignore

      val Y = Tie.id ()

      val op --> = ignore

      val exn = ()
      fun regExn0 c _ = regExn c
      fun regExn1 c _ _ = regExn c

      val list   = ignore
      val vector = ignore
      val array  = ignore
      val refc   = ignore

      val fixedInt = ()
      val largeInt = ()

      val largeReal = ()
      val largeWord = ()

      val bool   = ()
      val char   = ()
      val int    = ()
      val real   = ()
      val string = ()
      val word   = ()

      val word8  = ()
      val word32 = ()
(*
      val word64 = ()
*)

      fun hole () = ()

      open Arg DebugRep)

   open Layered
end
