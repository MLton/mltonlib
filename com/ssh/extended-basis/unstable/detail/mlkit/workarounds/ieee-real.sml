(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IEEEReal : IEEE_REAL = struct
   datatype rounding_mode =
      TO_NEAREST
    | TO_NEGINF
    | TO_POSINF
    | TO_ZERO

   datatype float_class =
      NAN
    | INF
    | ZERO
    | NORMAL
    | SUBNORMAL

   type decimal_approx =
        {class : float_class,
         sign : bool,
         digits : int list,
         exp : int}
end
