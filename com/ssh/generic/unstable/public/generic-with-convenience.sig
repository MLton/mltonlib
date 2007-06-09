(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature GENERIC_WITH_CONVENIENCE = sig
   include GENERICS GENERIC

   (** == Shorthands for Types with Labels or Constructors ==
    *
    * These should only be used for defining monomorphic type-indices.
    *)

   val C0' : String.t -> Unit.t Index.s
   val C1' : String.t -> 'a Index.t -> 'a Index.s

   val R' : String.t -> 'a Index.t -> ('a, Generics.Record.t) Index.p

   val regExn0 : Exn.t -> (Exn.t -> Unit.t Option.t) -> String.t Effect.t
   val regExn1 : ('a -> Exn.t) -> (Exn.t -> 'a Option.t) -> String.t -> 'a Index.t Effect.t

   (** == Tuples == *)

   val tuple2 : 'a Index.t * 'b Index.t
                -> ('a * 'b) Index.t
   val tuple3 : 'a Index.t * 'b Index.t * 'c Index.t
                -> ('a * 'b * 'c) Index.t
   val tuple4 : 'a Index.t * 'b Index.t * 'c Index.t * 'd Index.t
                -> ('a * 'b * 'c * 'd) Index.t

   (** == Integer Types ==
    *
    * WARNING: The encodings of sized integer types are not optimal for
    * serialization.  (They do work, however.)  For serialization, one
    * should encode sized integer types in terms of the corresponding
    * sized word types.
    *)

   val int32 : Int32.t Index.t
   val int64 : Int64.t Index.t

   (** == Some Standard Datatypes == *)

   val option : 'a Index.t -> 'a Option.t Index.t
   val order : order Index.t

   (** == Sums and Products == *)

   val &` : 'a Index.t * 'b Index.t -> ('a,'b) Product.t Index.t
   val |` : 'a Index.t * 'b Index.t -> ('a,'b) Sum.t Index.t

   (** == Abbreviations for Common Types == *)

   val sq : 'a Index.t -> ('a * 'a) Index.t
   val uop : 'a Index.t -> ('a -> 'a) Index.t
   val bop : 'a Index.t -> ('a * 'a -> 'a) Index.t
end
