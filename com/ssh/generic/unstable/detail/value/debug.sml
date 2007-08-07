(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDebug (Arg : OPEN_GENERIC) : OPEN_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   open Generics

   (* XXX Consider an asymptotically more efficient set representation. *)

   fun add1 kind (x, xs) =
       if List.exists (eq x) xs
       then fail (concat ["Duplicate ", kind, "s: ", x])
       else x::xs

   fun addN kind (xs, ys) = foldl (add1 kind) xs ys

   structure Check =
      LayerGenericRep (structure Outer = Arg.Rep
                       structure Closed = struct
                          type 'a t = Unit.t
                          type 'a s = String.t List.t
                          type ('a, 'k) p = String.t List.t
                       end)

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Check and Rep = Check.Closed

      val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` ? = addN "label" ?
      fun T () = []
      fun R l () = [Label.toString l]
      val tuple  = ignore
      val record = ignore

      fun op +` ? = addN "constructor" ?
      val unit = ()
      fun C0 c = [Con.toString c]
      fun C1 c () = [Con.toString c]
      val data = ignore

      val Y = Tie.id ()

      val op --> = ignore

      val exnCons : String.t List.t Ref.t = ref []
      fun regExn cs _ = exnCons := addN "exception constructor" (!exnCons, cs)
      val exn = ()

      val list   = ignore
      val vector = ignore
      val array  = ignore
      val refc   = ignore

      val largeInt  = ()
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
      val word64 = ())

   open Layered
end