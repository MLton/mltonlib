(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ty :> TY = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Product = struct
      datatype 'elem t = TIMES       of 'elem t Sq.t
                       | ELEM        of 'elem
                       | ISO_PRODUCT of 'elem t
   end

   structure Sum = struct
      datatype 'ty t = PLUS    of 'ty t Sq.t
                     | C0      of Generics.Con.t
                     | C1      of Generics.Con.t * 'ty
                     | ISO_SUM of 'ty t
   end

   structure Con0 = struct
      datatype t = BOOL | CHAR | EXN | FIXED_INT | INT | LARGE_INT
                 | LARGE_REAL | LARGE_WORD | REAL | STRING | UNIT | WORD
                 | WORD32 (*| WORD64*) | WORD8
   end

   structure Con1 = struct
      datatype t = ARRAY | LIST | REF | VECTOR
   end

   structure Con2 = struct
      datatype t = ARROW
   end

   open Product Sum Con0 Con1 Con2

   datatype 'var t = DATA   of 'var t Sum.t
                   | CON0   of Con0.t
                   | CON1   of Con1.t * 'var t
                   | CON2   of Con2.t * 'var t Sq.t
                   | FIX    of 'var * 'var t
                   | ISO    of 'var t
                   | RECORD of (Generics.Label.t * 'var t) Product.t
                   | TUPLE  of 'var t Product.t
                   | VAR    of 'var

   local
      fun product el =
       fn TIMES (l, r)    => product el l orelse product el r
        | ELEM t          => el t
        | ISO_PRODUCT p   => product el p
      fun sum ty =
       fn PLUS (l, r)     => sum ty l orelse sum ty r
        | C0 _            => false
        | C1 (_, t)       => ty t
        | ISO_SUM t       => sum ty t
      val rec ty =
       fn DATA s          => sum ty s
        | CON0 c          => c = EXN
        | CON1 (_, t)     => ty t
        | CON2 (ARROW, _) => false
        | FIX (_, t)      => ty t
        | ISO t           => ty t
        | RECORD r        => product (ty o #2) r
        | TUPLE t         => product ty t
        | VAR _           => false
   in
      val mayContainExn = ty
   end

   local
      fun product el =
       fn TIMES (l, r)    => product el l @ product el r
        | ELEM t          => el t
        | ISO_PRODUCT p   => product el p
      fun sum ty =
       fn PLUS (l, r)     => sum ty l @ sum ty r
        | C0 _            => []
        | C1 (_, t)       => ty t
        | ISO_SUM t       => sum ty t
      val rec ty =
       fn DATA s          => sum ty s
        | CON0 _          => []
        | CON1 (_, t)     => ty t
        | CON2 (ARROW, _) => []
        | FIX (v, t)      => List.filter (eq v) (ty t)
        | ISO t           => ty t
        | RECORD r        => product (ty o #2) r
        | TUPLE t         => product ty t
        | VAR v           => [v]
   in
      fun mayBeRecData t = not (null (ty t))
   end

   val isMutableType =
    fn CON1 (c, _) => ARRAY = c orelse REF = c
     | _           => false

   fun mayBeCyclic t =
       isMutableType t andalso (mayContainExn t orelse mayBeRecData t)
end
