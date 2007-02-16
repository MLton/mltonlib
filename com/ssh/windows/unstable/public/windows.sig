(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for Windows utilities.
 *
 * Parts of this signature follow the SML Basis Library specification:
 *
 *   http://mlton.org/basis/windows.html .
 *)
signature WINDOWS = sig
   structure Key : sig
      include BIT_FLAGS
      val allAccess : flags
      val createLink : flags
      val createSubKey : flags
      val enumerateSubKeys : flags
      val execute : flags
      val notify : flags
      val queryValue : flags
      val read : flags
      val setValue : flags
      val write : flags
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
      val createKeyEx : hkey * String.t * Key.flags -> create_result
      val deleteKey : (hkey * String.t) Effect.t
      val deleteValue : (hkey * String.t) Effect.t
      val enumKeyEx : hkey * Int.t -> String.t Option.t
      val enumValueEx : hkey * Int.t -> String.t Option.t
      val openKeyEx : hkey * String.t * Key.flags -> hkey

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

   structure EventLog : sig
      structure Type : sig
         include BIT_FLAGS
         val auditFailure : flags
         val auditSuccess : flags
         val error : flags
         val information : flags
         val warning : flags
      end
   end

   structure Module : sig
      type hmodule
      val getFileName : hmodule Option.t -> String.t
   end

   structure Path : sig
      val getShortName : String.t UnOp.t
   end
end
