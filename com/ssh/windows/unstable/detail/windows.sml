(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Implementation of Windows utilities. *)
structure Windows :> WINDOWS = struct
   exception Error of {function : String.t, error : Word32.t}

   val op >>& = With.>>&

   local
      fun `x = C.Get.ulong' (x ())
   in
      val errorSuccess     = `G_win_ERROR_SUCCESS.obj'
      val errorNoMoreItems = `G_win_ERROR_NO_MORE_ITEMS.obj'
      val errorMoreData    = `G_win_ERROR_MORE_DATA.obj'
   end

   fun errorToString error =
       With.around (fn () => F_win_FormatErrorLocalAlloc.f' error)
                   (ignore o F_win_LocalFree.f' o C.Ptr.inject')
                   ZString.toML'

   val () =
       Exn.addMessager
          (fn Error {function, error} =>
              SOME (concat ["Win.Error: ", function, " failed: ",
                            errorToString error])
            | _ => NONE)

   val getLastError = F_win_GetLastError.f

   fun raiseError function error =
       raise Error {function = function, error = error}

   fun raiseOnError function error = let
      val error = Word.fromInt error
   in
      if error = errorSuccess then () else raiseError function error
   end

   fun raiseLastError function =
       raiseError function (getLastError ())

   fun withAlloc alloc = With.around alloc C.free'
   fun withNew size = With.around (fn () => C.new' size) C.discard'
   fun withPtr f = withNew C.S.voidptr f
   fun withDword f = withNew C.S.ulong f
   fun withZs mlStr = withAlloc (fn () => ZString.dupML' mlStr)
   fun withBuf size = withAlloc (fn () => C.alloc' C.S.uchar size)

   exception InsufficientBuffer

   fun withDoublingBuf size f = let
      fun loop size = withBuf size (f /> size)
          handle InsufficientBuffer => loop (size * 0w2 + 0w1)
   in
      loop size
   end

   fun onError0ElseTruncatedSize function size f =
       (withDoublingBuf size)
          (fn (buf, size) => let
                 val result = f (buf, size)
              in
                 if 0w0 = result then raiseLastError function
                 else if size = result then raise InsufficientBuffer
                 else ZString.toML' buf
              end)

   fun onError0ElseRequiredSize function f = let
      val size = f (C.Ptr.null', 0w0)
   in
      if 0w0 = size
      then raiseLastError function
      else (withBuf size)
              (fn buf => let
                     val result = f (buf, size)
                  in
                     if 0w0 = result
                     then raiseLastError function
                     else ZString.toML' buf
                  end)
   end

   structure Key = struct
      open BitFlags
      local
         fun `x = SysWord.fromWord (C.Get.ulong' (x ()))
      in
         val allAccess        = `G_win_KEY_ALL_ACCESS.obj'
         val createLink       = `G_win_KEY_CREATE_LINK.obj'
         val createSubKey     = `G_win_KEY_CREATE_SUB_KEY.obj'
         val enumerateSubKeys = `G_win_KEY_ENUMERATE_SUB_KEYS.obj'
         val execute          = `G_win_KEY_EXECUTE.obj'
         val notify           = `G_win_KEY_NOTIFY.obj'
         val queryValue       = `G_win_KEY_QUERY_VALUE.obj'
         val read             = `G_win_KEY_READ.obj'
         val setValue         = `G_win_KEY_SET_VALUE.obj'
         val write            = `G_win_KEY_WRITE.obj'
      end
   end

   structure Reg = struct
      type hkey = C.voidptr

      local
         fun `x = C.Get.voidptr' (x ())
      in
         val classesRoot     = `G_win_HKEY_CLASSES_ROOT.obj'
         val currentConfig   = `G_win_HKEY_CURRENT_CONFIG.obj'
         val currentUser     = `G_win_HKEY_CURRENT_USER.obj'
         val dynData         = `G_win_HKEY_DYN_DATA.obj'
         val localMachine    = `G_win_HKEY_LOCAL_MACHINE.obj'
         val performanceData = `G_win_HKEY_PERFORMANCE_DATA.obj'
         val users           = `G_win_HKEY_USERS.obj'
      end

      val closeKey = raiseOnError "RegCloseKey" o F_win_RegCloseKey.f'

      datatype create_result
        = CREATED_NEW_KEY of hkey
        | OPENED_EXISTING_KEY of hkey

      fun createKeyEx (hKey, subKey, samDesired) =
          (withZs subKey >>& withPtr >>& withDword)
             (fn subKey & hkResult & dwDisposition =>
                 ((raiseOnError "RegCreateKeyEx")
                     (F_win_RegCreateKeyEx.f'
                         (hKey, subKey, 0w0, C.Ptr.null', 0w0,
                          SysWord.toWord samDesired, C.Ptr.null',
                          C.Ptr.|&! hkResult, C.Ptr.|&! dwDisposition))
                ; (if C.Get.ulong' dwDisposition =
                      C.Get.ulong' (G_win_REG_CREATED_NEW_KEY.obj' ())
                   then CREATED_NEW_KEY
                   else OPENED_EXISTING_KEY) (C.Get.voidptr' hkResult)))

      fun deleteKey (hKey, subKey) =
          (withZs subKey)
             (fn subKey =>
                 (raiseOnError "RegDeleteKey")
                    (F_win_RegDeleteKey.f' (hKey, subKey)))

      fun deleteValue (hKey, valueName) =
          (withZs valueName)
             (fn valueName =>
                 (raiseOnError "RegDeleteValue")
                    (F_win_RegDeleteValue.f' (hKey, valueName)))

      local
         fun mk function f (hKey, i) =
             if i < 0
             then raise Subscript
             else (withDword >>& withDoublingBuf 0w255)
                     (fn dwSize & (buf, size) => let
                            val () = C.Set.ulong' (dwSize, size)
                            val error =
                                Word.fromInt
                                   (f (hKey, Word.fromInt i, buf,
                                       C.Ptr.|&! dwSize, C.Ptr.null',
                                       C.Ptr.null', C.Ptr.null', C.Ptr.null'))
                         in
                            if error = errorMoreData then
                               raise InsufficientBuffer
                            else if error = errorNoMoreItems then
                               NONE
                            else if error = errorSuccess then
                               SOME (ZString.toML' buf)
                            else
                               raiseError function error
                         end)
      in
         val enumKeyEx = mk "RegEnumKeyEx" F_win_RegEnumKeyEx.f'
         val enumValueEx = mk "RegEnumValue" F_win_RegEnumValue.f'
      end

      fun openKeyEx (hKey, subKey, samDesired) =
          (withZs subKey >>& withPtr)
             (fn subKey & hkResult =>
                 ((raiseOnError "RegOpenKeyEx")
                     (F_win_RegOpenKeyEx.f'
                         (hKey, subKey, 0w0, SysWord.toWord samDesired,
                          C.Ptr.|&! hkResult))
                ; C.Get.voidptr' hkResult))

      datatype value
        = BINARY of Word8Vector.t
        | DWORD of Word32.t
        | EXPAND_SZ of String.t
        | MULTI_SZ of String.t List.t
        | QWORD of Word64.t
        | SZ of String.t

      local
         local
            fun `x = C.Get.ulong' (x ())
         in
            val binary   = `G_win_REG_BINARY.obj'
            val dword    = `G_win_REG_DWORD.obj'
            val expandSz = `G_win_REG_EXPAND_SZ.obj'
            val multiSz  = `G_win_REG_MULTI_SZ.obj'
            val qword    = `G_win_REG_QWORD.obj'
            val sz       = `G_win_REG_SZ.obj'
         end

         val toMultiSz = String.tokens (#"\000" <\ op =) o Byte.bytesToString
         val toSz = hd o toMultiSz

         fun fromBin ty =
             if      ty = binary   then BINARY
             else if ty = dword    then DWORD o Word32.fromLittleBytes
             else if ty = expandSz then EXPAND_SZ o toSz
             else if ty = multiSz  then MULTI_SZ o toMultiSz
             else if ty = qword    then QWORD o Word64.fromLittleBytes
             else if ty = sz       then SZ o toSz
             else raise Fail "Unsupported RegQueryValueEx functionality"

         val toBin =
          fn BINARY x => (binary, x)
           | DWORD x => (dword, Word32.toLittleBytes x)
           | EXPAND_SZ x => (expandSz, Byte.stringToBytes (x ^ "\000"))
           | MULTI_SZ x =>
             (multiSz,
              Byte.stringToBytes
                 (concat (map (op ^ /> "\000") x @ ["\000\000"])))
           | QWORD x => (qword, Word64.toLittleBytes x)
           | SZ x => (sz, Byte.stringToBytes (x ^ "\000"))
      in
         fun queryValueEx (hKey, valueName) =
             (withZs valueName >>& withDword >>& withDword)
                (fn valueName & dwType & dwSize => let
                       fun f buf =
                           F_win_RegQueryValueEx.f'
                              (hKey, valueName, C.Ptr.null', C.Ptr.|&! dwType,
                               buf, C.Ptr.|&! dwSize)
                    in
                       raiseOnError "RegQueryValueEx" (f C.Ptr.null')
                     ; (SOME o withBuf (C.Get.ulong' dwSize))
                          (fn buf =>
                              (raiseOnError "RegQueryValueEx" (f buf)
                             ; (fromBin (C.Get.ulong' dwType) o
                                Word8Vector.tabulate)
                                  (Word.toInt (C.Get.ulong' dwSize),
                                   C.Get.uchar' o buf <\ C.Ptr.sub' C.S.uchar)))
                    end)

         fun setValueEx (hKey, valueName, value) = let
            val (ty, data) = toBin value
            val size = Word.fromInt (Word8Vector.length data)
         in
            (withZs valueName >>& withBuf size)
               (fn valueName & buf =>
                   (Word8Vector.appi
                       (fn (i, x) =>
                           C.Set.uchar' (C.Ptr.sub' C.S.uchar (buf, i), x)) data
                  ; (raiseOnError "RegSetValueEx")
                       (F_win_RegSetValueEx.f'
                           (hKey, valueName, 0w0, ty, C.Ptr.ro' buf, size))))
         end
      end
   end

   structure Module = struct
      type hmodule = C.voidptr

      val null = C.Ptr.null'

      fun getFileName module =
          (onError0ElseTruncatedSize "GetModuleFileName" 0w255)
             (fn (b, s) => F_win_GetModuleFileName.f' (module, b, s))
   end

   structure Path = struct
      fun getShortName path =
          (withZs path)
             (fn path =>
                 (onError0ElseRequiredSize "GetShortPathName")
                    (fn (b, s) => F_win_GetShortPathName.f' (path, b, s)))
   end
end
