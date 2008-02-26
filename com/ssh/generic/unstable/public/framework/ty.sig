(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Ty} module that defines a datatype corresponding to
 * type representation expressions.
 *)
signature TY = sig
   structure Product : sig
      datatype 'elem t = TIMES       of 'elem t Sq.t
                       | ELEM        of 'elem
                       | ISO_PRODUCT of 'elem t
   end

   structure Sum : sig
      datatype 'ty t = PLUS    of 'ty t Sq.t
                     | C0      of Generics.Con.t
                     | C1      of Generics.Con.t * 'ty
                     | ISO_SUM of 'ty t
   end


   structure Con0 : sig
      datatype t = BOOL | CHAR | EXN | FIXED_INT | INT | LARGE_INT
                 | LARGE_REAL | LARGE_WORD | REAL | STRING | UNIT | WORD
                 | WORD32 (*| WORD64*) | WORD8
   end

   structure Con1 : sig
      datatype t = ARRAY | LIST | REF | VECTOR
   end

   structure Con2 : sig
      datatype t = ARROW
   end

   datatype 'var t = DATA   of 'var t Sum.t
                   | CON0   of Con0.t
                   | CON1   of Con1.t * 'var t
                   | CON2   of Con2.t * 'var t Sq.t
                   | FIX    of 'var * 'var t
                   | ISO    of 'var t
                   | RECORD of (Generics.Label.t * 'var t) Product.t
                   | TUPLE  of 'var t Product.t
                   | VAR    of 'var

   (** == Data Recursion Info ==
    *
    * These correspond to the algorithms in the {DataRecInfo} generic
    * and have been implemented here as recursive algorithms on type
    * representation expressions with the intention of documenting their
    * semantics.
    *
    * The {DataRecInfo} generic computes the same information
    * incrementally during type representation construction and is likely
    * to be more amenable to compiler optimizations such as constant
    * folding.
    *)

   val isMutableType :  'a t UnPr.t
   val mayBeCyclic   : ''a t UnPr.t
   val mayBeRecData  : ''a t UnPr.t
   val mayContainExn :  'a t UnPr.t
end
