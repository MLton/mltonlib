(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Top-Level Bindings == *)

(** === Datatypes === *)

datatype product = datatype Product.product
datatype sum = datatype Sum.sum

(** === Functions === *)

(** ==== Exn ==== *)

val finally = Exn.finally
val try = Exn.try

(** ==== Fn ==== *)

val const = Fn.const
val curry = Fn.curry
val failing = Fn.failing
val flip = Fn.flip
val id = Fn.id
val pass = Fn.pass
val uncurry = Fn.uncurry
val undefined = Fn.undefined
val op /> = Fn./>
val op </ = Fn.</
val op <\ = Fn.<\
val op >| = Fn.>|
val op \> = Fn.\>
val op |< = Fn.|<

(** ==== Option ==== *)

val isNone = Option.isNone

(** ==== Ref ==== *)

val op :=: = Ref.:=:

(** ==== UnPr ==== *)

val andAlso = UnPr.andAlso
val negate = UnPr.negate
val orElse = UnPr.orElse
