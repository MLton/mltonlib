(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Formattable == *)

signature FORMATTABLE = sig
   type formattable
   type formattable_format
   val fmt : formattable_format -> formattable -> String.t
end

(** == Formattable and Scannable == *)

signature FORMATTABLE_and_SCANNABLE = sig
   include FORMATTABLE SCANNABLE
   sharing type scannable = formattable
end

signature FORMATTABLE_and_SCANNABLE_FROM_FORMAT = sig
   include FORMATTABLE SCANNABLE_FROM_FORMAT
   sharing type scannable = formattable
   sharing type scannable_format = formattable_format
end
