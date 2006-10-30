(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature CONTROL_STRUCTS =
   sig
      structure Region: REGION
   end

signature CONTROL =
   sig
      include CONTROL_STRUCTS

      val acceptMozillaExtensions: bool ref
      val errorStr: Region.t * string -> unit
   end
