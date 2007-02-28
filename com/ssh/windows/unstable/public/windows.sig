(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for Windows utilities.
 *
 * Aside from a few minor extensions, this signature specifies a subset of
 * the [http://mlton.org/basis/windows.html Windows structure] in the
 * Standard ML Basis Library.
 *)
signature WINDOWS = sig
   structure Key : sig
      include FLAGS where type flags_word = Word32.t
      type t = flags
      val allAccess : t
      val createLink : t
      val createSubKey : t
      val enumerateSubKeys : t
      val execute : t
      val notify : t
      val queryValue : t
      val read : t
      val setValue : t
      val write : t
   end

   structure Reg : sig
      eqtype hkey
      val classesRoot : hkey
      val currentConfig : hkey
      val currentUser : hkey
      val dynData : hkey
      val localMachine : hkey
      val performanceData : hkey
      val users : hkey

      datatype create_result
        = CREATED_NEW_KEY of hkey
        | OPENED_EXISTING_KEY of hkey
      val keyOf : create_result -> hkey

      val closeKey : hkey Effect.t
      val createKeyEx : hkey * String.t * Key.t -> create_result
      val deleteKey : (hkey * String.t) Effect.t
      val deleteValue : (hkey * String.t) Effect.t
      val enumKeyEx : hkey * Int.t -> String.t Option.t
      val enumValueEx : hkey * Int.t -> String.t Option.t
      val openKeyEx : hkey * String.t * Key.t -> hkey

      datatype value
        = BINARY of Word8Vector.t
        | DWORD of Word32.t
        | EXPAND_SZ of String.t
        | MULTI_SZ of String.t List.t
        | QWORD of Word64.t
        | SZ of String.t
      val queryValueEx : hkey * String.t -> value Option.t
      val setValueEx : (hkey * String.t * value) Effect.t
   end
end
