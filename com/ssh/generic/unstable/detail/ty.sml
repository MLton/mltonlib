(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ty :> TY = struct
   structure Product = struct
      datatype 'elem t = *`   of 'elem t Sq.t
                       | ELEM of 'elem
                       | ISO  of 'elem t
   end

   structure Sum = struct
      datatype 'ty t = +`  of 'ty t Sq.t
                     | C0  of Generics.Con.t
                     | C1  of Generics.Con.t * 'ty
                     | ISO of 'ty t
   end

   structure Var = struct
      type t = Unit.t Ref.t
      fun new () = ref ()
   end

   structure Con0 = struct
      datatype t = BOOL | CHAR | EXN | FIXED_INT | INT | LARGE_INT
                 | LARGE_REAL | LARGE_WORD | REAL | STRING | UNIT | WORD
                 | WORD32 | WORD64 | WORD8
   end

   structure Con1 = struct
      datatype t = ARRAY | LIST | REF | VECTOR
   end

   structure Con2 = struct
      datatype t = ARROW
   end

   datatype 'var t =
            DATA   of 'var t Sum.t
          | CON0   of Con0.t
          | CON1   of Con1.t * 'var t
          | CON2   of Con2.t * 'var t Sq.t
          | FIX    of 'var * 'var t
          | ISO    of 'var t
          | RECORD of (Generics.Label.t * 'var t) Product.t
          | TUPLE  of 'var t Product.t
          | VAR    of 'var
end
