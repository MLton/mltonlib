(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Extended {OS} signature.
 *
 * This signature is just a restatement of the {OS} signature using the
 * extended substructure signatures.
 *)
signature OS = sig
    structure FileSys : OS_FILE_SYS
    structure IO : BASIS_OS_IO
    structure Path : BASIS_OS_PATH
    structure Process : BASIS_OS_PROCESS

    eqtype syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror option 
end
