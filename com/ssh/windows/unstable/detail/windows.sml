(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX make pretty printing of args in error messages a compile time option *)

(* Implementation of Windows utilities. *)
structure Windows :> WINDOWS_EX = struct
   local
      open Type Prettier
   in
      fun F name args =
          pretty NONE
                 ((group o nest 2)
                     (txt "Windows." <^> txt name <$>
                      (parens o group o nest 1 o fillSep o punctuate comma)
                      args))
      val A = layout
      val str = string
      val ptr = iso word32
                    let open MLRep.Long.Unsigned
                    in (C.Cvt.ml_ulong o C.U.p2i, C.U.i2p o C.Cvt.c_ulong)
                    end
      val opt = option
      val int = int
      val sw = word64
   end

   val op >>& = With.>>&

   local
      fun `x = C.Get.ulong' (x ())
   in
      val errorSuccess     = `G_win_ERROR_SUCCESS.obj'
      val errorNoMoreItems = `G_win_ERROR_NO_MORE_ITEMS.obj'
      val errorMoreData    = `G_win_ERROR_MORE_DATA.obj'
   end

   val getLastError = F_win_GetLastError.f

   fun raiseError call error =
       raise OS.SysErr
                (concat
                    [call (), ": ",
                     With.around (fn () => F_win_FormatErrorLocalAlloc.f' error)
                                 (ignore o F_win_LocalFree.f' o C.Ptr.inject')
                                 ZString.toML'],
                 NONE)

   fun raiseOnError call error = let
      val error = Word.fromInt error
   in
      if error = errorSuccess then () else raiseError call error
   end

   fun raiseLastError call =
       raiseError call (getLastError ())

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

   fun onError0ElseTruncatedSize call size f =
       (withDoublingBuf size)
          (fn (buf, size) => let
                 val result = f (buf, size)
              in
                 if 0w0 = result then raiseLastError call
                 else if size = result then raise InsufficientBuffer
                 else ZString.toML' buf
              end)

   fun onError0ElseRequiredSize call f = let
      val size = f (C.Ptr.null', 0w0)
   in
      if 0w0 = size
      then raiseLastError call
      else (withBuf size)
              (fn buf => let
                     val result = f (buf, size)
                  in
                     if 0w0 = result
                     then raiseLastError call
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

      fun closeKey h =
          raiseOnError (fn () => F"Reg.closeKey"[A ptr h])
                       (F_win_RegCloseKey.f' h)

      datatype create_result
        = CREATED_NEW_KEY of hkey
        | OPENED_EXISTING_KEY of hkey

      val keyOf = fn CREATED_NEW_KEY k => k | OPENED_EXISTING_KEY k => k

      fun createKeyEx (h, n, m) =
          (withZs n >>& withPtr >>& withDword)
             (fn n' & hkResult & dwDisposition =>
                 (raiseOnError
                     (fn () => F"Reg.createKeyEx"[A ptr h, A str n, A sw m])
                     (F_win_RegCreateKeyEx.f'
                         (h, n', 0w0, C.Ptr.null', 0w0,
                          SysWord.toWord m, C.Ptr.null',
                          C.Ptr.|&! hkResult, C.Ptr.|&! dwDisposition))
                ; (if C.Get.ulong' dwDisposition =
                      C.Get.ulong' (G_win_REG_CREATED_NEW_KEY.obj' ())
                   then CREATED_NEW_KEY
                   else OPENED_EXISTING_KEY) (C.Get.voidptr' hkResult)))

      fun deleteKey (h, n) =
          (withZs n)
             (fn n' =>
                 raiseOnError
                    (fn () => F"Reg.deleteKey"[A ptr h, A str n])
                    (F_win_RegDeleteKey.f' (h, n')))

      fun deleteValue (h, n) =
          (withZs n)
             (fn n' =>
                 raiseOnError
                    (fn () => F"Reg.deleteValue"[A ptr h, A str n])
                    (F_win_RegDeleteValue.f' (h, n')))

      local
         fun mk name f (h, i) =
             if i < 0
             then raise Subscript
             else (withDword >>& withDoublingBuf 0w255)
                     (fn dwSize & (buf, size) => let
                            val () = C.Set.ulong' (dwSize, size)
                            val error =
                                Word.fromInt
                                   (f (h, Word.fromInt i, buf,
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
                               raiseError
                                  (fn () => F name [A ptr h, A int i])
                                  error
                         end)
      in
         val enumKeyEx = mk "Reg.enumKeyEx" F_win_RegEnumKeyEx.f'
         val enumValueEx = mk "Reg.enumValueEx" F_win_RegEnumValue.f'
      end

      fun openKeyEx (h, n, m) =
          (withZs n >>& withPtr)
             (fn n' & hkResult =>
                 (raiseOnError
                     (fn () => F"Reg.openKeyEx"[A ptr h, A str n, A sw m])
                     (F_win_RegOpenKeyEx.f'
                         (h, n', 0w0, SysWord.toWord m,
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
         fun queryValueEx (h, n) =
             (withZs n >>& withDword >>& withDword)
                (fn n' & dwType & dwSize => let
                       fun f buf =
                           F_win_RegQueryValueEx.f'
                              (h, n', C.Ptr.null', C.Ptr.|&! dwType,
                               buf, C.Ptr.|&! dwSize)
                       fun call () = F"Reg.queryValueEx"[A ptr h, A str n]
                    in
                       raiseOnError call (f C.Ptr.null')
                     ; (SOME o withBuf (C.Get.ulong' dwSize))
                          (fn buf =>
                              (raiseOnError call (f buf)
                             ; (fromBin (C.Get.ulong' dwType) o
                                Word8Vector.tabulate)
                                  (Word.toInt (C.Get.ulong' dwSize),
                                   C.Get.uchar' o buf <\ C.Ptr.sub' C.S.uchar)))
                    end)

         fun setValueEx (h, n, v) = let
            val (ty, data) = toBin v
            val size = Word.fromInt (Word8Vector.length data)
         in
            (withZs n >>& withBuf size)
               (fn n' & buf =>
                   (Word8Vector.appi
                       (fn (i, x) =>
                           C.Set.uchar' (C.Ptr.sub' C.S.uchar (buf, i), x)) data
                  ; raiseOnError
                       (fn () => F"Reg.setValueEx"[A ptr h, A str n,
                                                   Prettier.txt "<value>"])
                       (F_win_RegSetValueEx.f'
                           (h, n', 0w0, ty, C.Ptr.ro' buf, size))))
         end
      end
   end

   structure EventLog = struct
      structure Type = struct
         open BitFlags
         val ` = SysWord.fromInt o MLRep.Short.Unsigned.toIntX o C.Get.ushort' o
                 pass ()
         val auditFailure = `G_win_EVENTLOG_AUDIT_FAILURE.obj'
         val auditSuccess = `G_win_EVENTLOG_AUDIT_SUCCESS.obj'
         val error        = `G_win_EVENTLOG_ERROR_TYPE.obj'
         val information  = `G_win_EVENTLOG_INFORMATION_TYPE.obj'
         val warning      = `G_win_EVENTLOG_WARNING_TYPE.obj'
      end
   end

   structure Module = struct
      type t = C.voidptr

      fun getFileName m = let
         val m' = getOpt (m, C.Ptr.null')
      in
         onError0ElseTruncatedSize
            (fn () => F"Module.getFileName"[A (opt ptr) m])
            0w255
            (fn (b, s) => F_win_GetModuleFileName.f' (m', b, s))
      end
   end

   structure Path = struct
      fun getShortName p =
          (withZs p)
             (fn p' =>
                 onError0ElseRequiredSize
                    (fn () => F"Path.getShortName"[A str p])
                    (fn (b, s) => F_win_GetShortPathName.f' (p', b, s)))
   end

   structure Wait = struct
      type t = C.voidptr

      datatype 'a result
        = ABANDONED of 'a
        | OBJECT of 'a
        | TIMEOUT

      val any = undefined
      val all = undefined
   end

   structure Semaphore = struct
      type t = C.voidptr
      val create = undefined
      val close = undefined
      val release = undefined
      val toWait = undefined
   end

   structure Mutex = struct
      type t = C.voidptr
      val create = undefined
      val close = undefined
      val toWait = undefined
   end

   structure Timer = struct
      type t = C.voidptr
      val create = undefined
      val close = undefined
      val set = undefined
      val cancel = undefined
      val toWait = undefined
   end

   structure FileChange = struct
      structure Filter = struct
         open BitFlags
         fun `x = SysWord.fromWord (C.Get.ulong' (x ()))
         val attributes = `G_win_FILE_NOTIFY_CHANGE_ATTRIBUTES.obj'
         val dirName    = `G_win_FILE_NOTIFY_CHANGE_DIR_NAME.obj'
         val fileName   = `G_win_FILE_NOTIFY_CHANGE_FILE_NAME.obj'
         val lastWrite  = `G_win_FILE_NOTIFY_CHANGE_LAST_WRITE.obj'
         val security   = `G_win_FILE_NOTIFY_CHANGE_SECURITY.obj'
         val size       = `G_win_FILE_NOTIFY_CHANGE_SIZE.obj'
      end

      type t = C.voidptr
      val first = undefined
      val next = undefined
      val close = undefined
      val toWait = undefined
   end
end
