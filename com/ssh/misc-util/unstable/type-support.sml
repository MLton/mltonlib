(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Supporting primitives required by {TYPE}
 *)

structure TypeSupport :> sig
   eqtype label
   eqtype constructor

   type record
   type tuple

   val labelToString : label -> String.t
   val constructorToString : constructor -> String.t

   val L : String.t -> label
   val C : String.t -> constructor
end = struct
   structure Dbg = MkDbg (open DbgDefs val name = "TypeSupport")

   type label = String.t
   type constructor = String.t

   type record = Unit.t
   type tuple = Unit.t

   val labelToString = id
   val constructorToString = id

   val L = Effect.obs (fn s => Dbg.assert 0 (fn () => SmlSyntax.isLabel s))
   val C = Effect.obs (fn s => Dbg.assert 0 (fn () => SmlSyntax.isLongId s))
end
