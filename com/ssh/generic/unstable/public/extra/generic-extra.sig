(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 * Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for frequently used derived type representations.
 *)
signature GENERIC_EXTRA = sig
   include GENERICS GENERIC

   (** == Shorthands for Types with Labels or Constructors ==
    *
    * These should only be used for defining monomorphic representations.
    *)

   val C0' : String.t -> Unit.t Rep.s
   val C1' : String.t -> ('a, 'x) Open.Rep.t -> 'a Rep.s

   val R' : String.t -> ('a, 'x) Open.Rep.t -> ('a, Record.t) Rep.p

   val regExn0' : String.t -> Exn.t -> (Exn.t -> Unit.t) Effect.t
   val regExn1' : String.t -> ('a, 'x) Open.Rep.t
                  -> ('a -> Exn.t) -> (Exn.t -> 'a) Effect.t

   (** == Shorthands for Types with an Isomorphism == *)

   val data' : ('b, 'y) Open.Rep.s -> ('a, 'b) Iso.t -> 'a Rep.t
   val record' : ('b, Record.t, 'y) Open.Rep.p -> ('a, 'b) Iso.t -> 'a Rep.t
   val tuple' : ('b, Tuple.t, 'y) Open.Rep.p -> ('a, 'b) Iso.t -> 'a Rep.t

   (** == Tuples ==
    *
    * Note that these are provided for convenience --- generics are not
    * limited to these tuple arities.  To encode an arbitrary n-tuple, use
    * the following pattern:
    *
    *> fun tupleN (t1, ..., tN) =
    *>     tuple' (T t1 *` ... *` T tN)
    *>            (fn (v1, ..., vN) => (v1 & ... & vN),
    *>             fn (v1 & ... & vN) => (v1, ..., vN))
    *)

   val tuple2 : ('a, 's) Open.Rep.t *
                ('b, 't) Open.Rep.t -> ('a * 'b) Rep.t
   val tuple3 : ('a, 's) Open.Rep.t *
                ('b, 't) Open.Rep.t *
                ('c, 'u) Open.Rep.t -> ('a * 'b * 'c) Rep.t
   val tuple4 : ('a, 's) Open.Rep.t *
                ('b, 't) Open.Rep.t *
                ('c, 'u) Open.Rep.t *
                ('d, 'v) Open.Rep.t -> ('a * 'b * 'c * 'd) Rep.t

   (** == Integer Types == *)

   val int32 : Int32.t Rep.t
(*
   val int64 : Int64.t Rep.t
*)

   val position : Position.t Rep.t

   (** == Some Standard Datatypes == *)

   val option : ('a, 'x) Open.Rep.t -> 'a Option.t Rep.t
   val order : Order.t Rep.t

   (** == Binary Sums and Products ==
    *
    * Note that the following are not the same as the {*`} and {+`}
    * combinators for encoding n-ary products and sums.  Rather, the
    * following encode the particular general purpose binary product
    * and sum types provided by the Extended Basis library.
    *)

   val &` : ('a, 'x) Open.Rep.t *
            ('b, 'y) Open.Rep.t -> ('a, 'b) Product.t Rep.t
   val |` : ('a, 'x) Open.Rep.t *
            ('b, 'y) Open.Rep.t -> ('a, 'b) Sum.t Rep.t

   (** == Abbreviations for Common Types == *)

   val sq : ('a, 'x) Open.Rep.t -> 'a Sq.t Rep.t
   val unOp : ('a, 'x) Open.Rep.t -> 'a UnOp.t Rep.t
   val binOp : ('a, 'x) Open.Rep.t -> 'a BinOp.t Rep.t
end
