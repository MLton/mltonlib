(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Scannable == *)

signature SCANNABLE_CORE = sig
   type scannable
   val scan : (Char.t, 's) Reader.t -> (scannable, 's) Reader.t
end

signature SCANNABLE = SCANNABLE_CORE

(** == Scannable from Format == *)

signature SCANNABLE_FROM_FORMAT_CORE = sig
   type scannable
   type scannable_format
   val scan :
       scannable_format -> (Char.t, 's) Reader.t -> (scannable, 's) Reader.t
end

signature SCANNABLE_FROM_FORMAT = SCANNABLE_FROM_FORMAT_CORE
