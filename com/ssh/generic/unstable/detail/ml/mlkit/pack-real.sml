(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure PackLargeRealLittle : PACK_REAL where type real = LargeReal.t =
   PackRealLittle
structure PackLargeRealBig    : PACK_REAL where type real = LargeReal.t =
   PackRealBig
