-*- sml -*-

#include <windows.h>

(* begin *)
(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Windows = struct

#define C_CODE(_)

#define WIN_CONST(name, type) val wc_##name = wt_##type <name>

fun wt_DWORD  x : Word32.t  = x
fun wt_WORD   x : Word16.t  = x
fun wt_REGSAM x : Word32.t  = x
fun wt_HKEY   x : C.voidptr =
    C_Int.mk_voidptr let open MLton.Pointer in add (null, x) end

#include "detail/ffi/windows.h"

end
(* end *)
