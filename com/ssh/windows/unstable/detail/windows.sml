(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX make pretty printing of args in error messages a compile time option *)

(* Implementation of Windows utilities. *)
structure Windows :> WINDOWS_EX = struct
   open Windows

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
      val lst = list
      val ptr = iso word32
                    let open MLRep.Long.Unsigned
                    in (C.Cvt.ml_ulong o C.U.p2i, C.U.i2p o C.Cvt.c_ulong)
                    end
      val opt = option
      val int = int
      val w32 = word32
      val bool = bool
      val time = iso largeReal (Time.toReal, Time.fromReal)
   end

   local
      open With
   in
      val one = one
      val around = around
      val op >>& = Monad.>>&
   end

   val success     = wc_ERROR_SUCCESS
   val noMoreItems = wc_ERROR_NO_MORE_ITEMS
   val moreData    = wc_ERROR_MORE_DATA

   val getLastError = F_win_GetLastError.f

   fun raiseError call e =
       raise OS.SysErr
                (concat
                    [call (), ": ",
                     one (around (fn () => F_win_FormatErrorLocalAlloc.f' e)
                                 (ignore o F_win_LocalFree.f' o C.Ptr.inject'))
                         ZString.toML'],
                 NONE)

   fun raiseOnError call f x = let
      val e = Word.fromInt (f x)
   in
      if e = success then () else raiseError call e
   end

   fun raiseLastError call =
       raiseError call (getLastError ())

   fun raiseOn isFailure toResult call f x = let
      val r = f x
   in
      if isFailure r then raiseLastError call else toResult r
   end

   val null = C.Ptr.null'
   val toCBool = fn true => 1 | false => 0

   fun raiseOnNull ? = raiseOn C.Ptr.isNull' id ?
   fun raiseOnFalse ? = raiseOn (0 <\ op =) ignore ?

   fun raiseOnNullIfErrorElseNone call f x = let
      val r = f x
   in
      if C.Ptr.isNull' r
      then let
            val e = getLastError ()
         in
            if e = success then NONE else raiseError call e
         end
      else SOME r
   end

   fun ptrToBool name f h = raiseOnFalse (fn () => F name [A ptr h]) f h

   fun withAlloc alloc = around alloc C.free'
   fun withNew size = around (fn () => C.new' size) C.discard'
   val withPtr = withNew C.S.voidptr
   val withDword = withNew C.S.ulong
   val withLong = withNew C.S.slong
   fun withZs mlStr = withAlloc (fn () => ZString.dupML' mlStr)
   val withOptZs = fn NONE => With.return null | SOME s => withZs s
   fun withBuf size = withAlloc (fn () => C.alloc' C.S.uchar size)

   exception InsufficientBuffer

   fun withDoublingBuf size = let
      fun loop size f = one (withBuf size) (f /> size)
          handle InsufficientBuffer => loop (size * 0w2 + 0w1) f
   in
      With.lift (loop size)
   end

   fun onError0ElseTruncatedSize call s f =
       one (withDoublingBuf s)
           (fn (b, s) => let
                  val r = f (b, s)
               in
                  if 0w0 = r then raiseLastError call
                  else if s = r then raise InsufficientBuffer
                  else ZString.toML' b
               end)

   fun onError0ElseRequiredSize call f = let
      val s = f (null, 0w0)
   in
      if 0w0 = s then raiseLastError call else
      one (withBuf s)
          (fn b => let
                 val r = f (b, s)
              in
                 if 0w0 = r then raiseLastError call else ZString.toML' b
              end)
   end

   structure Key = struct
      open Word32Flags
      type t = flags
      val allAccess        = wc_KEY_ALL_ACCESS
      val createLink       = wc_KEY_CREATE_LINK
      val createSubKey     = wc_KEY_CREATE_SUB_KEY
      val enumerateSubKeys = wc_KEY_ENUMERATE_SUB_KEYS
      val execute          = wc_KEY_EXECUTE
      val notify           = wc_KEY_NOTIFY
      val queryValue       = wc_KEY_QUERY_VALUE
      val read             = wc_KEY_READ
      val setValue         = wc_KEY_SET_VALUE
      val write            = wc_KEY_WRITE
   end

   structure Reg = struct
      type hkey = C.voidptr

      val classesRoot     = wc_HKEY_CLASSES_ROOT
      val currentConfig   = wc_HKEY_CURRENT_CONFIG
      val currentUser     = wc_HKEY_CURRENT_USER
      val dynData         = wc_HKEY_DYN_DATA
      val localMachine    = wc_HKEY_LOCAL_MACHINE
      val performanceData = wc_HKEY_PERFORMANCE_DATA
      val users           = wc_HKEY_USERS

      fun closeKey h =
          raiseOnError (fn () => F"Reg.closeKey"[A ptr h])
                       F_win_RegCloseKey.f' h

      datatype create_result
        = CREATED_NEW_KEY of hkey
        | OPENED_EXISTING_KEY of hkey

      val keyOf = fn CREATED_NEW_KEY k => k | OPENED_EXISTING_KEY k => k

      fun createKeyEx (h, n, m) =
          one (withZs n >>& withPtr >>& withDword)
              (fn n' & hkResult & dwDisposition =>
                  (raiseOnError
                      (fn () => F"Reg.createKeyEx"[A ptr h, A str n, A w32 m])
                      F_win_RegCreateKeyEx.f'
                      (h, n', 0w0, null, 0w0, m, null, C.Ptr.|&! hkResult,
                       C.Ptr.|&! dwDisposition)
                 ; (if C.Get.ulong' dwDisposition = wc_REG_CREATED_NEW_KEY
                    then CREATED_NEW_KEY
                    else OPENED_EXISTING_KEY) (C.Get.voidptr' hkResult)))

      fun deleteKey (h, n) =
          one (withZs n)
              (fn n' =>
                  raiseOnError
                     (fn () => F"Reg.deleteKey"[A ptr h, A str n])
                     F_win_RegDeleteKey.f' (h, n'))

      fun deleteValue (h, n) =
          one (withZs n)
              (fn n' =>
                  raiseOnError
                     (fn () => F"Reg.deleteValue"[A ptr h, A str n])
                     F_win_RegDeleteValue.f' (h, n'))

      local
         fun mk name f (h, i) =
             if i < 0 then raise Subscript else
             one (withDword >>& withDoublingBuf 0w255)
                 (fn s & (b, l) => let
                        val () = C.Set.ulong' (s, l)
                        val e = Word.fromInt
                                   (f (h, Word.fromInt i, b, C.Ptr.|&! s, null,
                                       null, null, null))
                     in
                        if      e = moreData    then raise InsufficientBuffer
                        else if e = noMoreItems then NONE
                        else if e = success     then SOME (ZString.toML' b)
                        else raiseError (fn () => F name [A ptr h, A int i]) e
                     end)
      in
         val enumKeyEx = mk "Reg.enumKeyEx" F_win_RegEnumKeyEx.f'
         val enumValueEx = mk "Reg.enumValueEx" F_win_RegEnumValue.f'
      end

      fun openKeyEx (h, n, m) =
          one (withZs n >>& withPtr)
              (fn n' & r =>
                  (raiseOnError
                      (fn () => F"Reg.openKeyEx"[A ptr h, A str n, A w32 m])
                      F_win_RegOpenKeyEx.f' (h, n', 0w0, m, C.Ptr.|&! r)
                 ; C.Get.voidptr' r))

      datatype value
        = BINARY of Word8Vector.t
        | DWORD of Word32.t
        | EXPAND_SZ of String.t
        | MULTI_SZ of String.t List.t
        | QWORD of Word64.t
        | SZ of String.t

      local
         val binary   = wc_REG_BINARY
         val dword    = wc_REG_DWORD
         val expandSz = wc_REG_EXPAND_SZ
         val multiSz  = wc_REG_MULTI_SZ
         val qword    = wc_REG_QWORD
         val sz       = wc_REG_SZ

         val toMultiSz = String.tokens (#"\000" <\ op =) o Byte.bytesToString
         val toSz = hd o toMultiSz

         fun fromBin ty =
             if      ty = binary   then BINARY
             else if ty = dword    then DWORD o Word32.fromLittleBytes
             else if ty = expandSz then EXPAND_SZ o toSz
             else if ty = multiSz  then MULTI_SZ o toMultiSz
             else if ty = qword    then QWORD o Word64.fromLittleBytes
             else if ty = sz       then SZ o toSz
             else fail "Unsupported RegQueryValueEx functionality"

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
             one (withZs n >>& withDword >>& withDword)
                 (fn n' & t & s => let
                        fun f b =
                            raiseOnError
                               (fn () => F"Reg.queryValueEx"[A ptr h, A str n])
                               F_win_RegQueryValueEx.f'
                               (h, n', null, C.Ptr.|&! t, b, C.Ptr.|&! s)
                     in
                        f null
                      ; (SOME o one (withBuf (C.Get.ulong' s)))
                           (fn b =>
                               (f b
                              ; (fromBin (C.Get.ulong' t) o
                                 Word8Vector.tabulate)
                                   (Word.toInt (C.Get.ulong' s),
                                    C.Get.uchar' o b <\ C.Ptr.sub' C.S.uchar)))
                     end)

         fun setValueEx (h, n, v) = let
            val (t, d) = toBin v
            val s = Word.fromInt (Word8Vector.length d)
         in
            one (withZs n >>& withBuf s)
                (fn n' & b =>
                    (Word8Vector.appi
                        (fn (i, x) =>
                            C.Set.uchar' (C.Ptr.sub' C.S.uchar (b, i), x))
                        d
                      ; raiseOnError
                           (fn () => F"Reg.setValueEx"[A ptr h, A str n,
                                                       Prettier.txt "<value>"])
                           F_win_RegSetValueEx.f'
                           (h, n', 0w0, t, C.Ptr.ro' b, s)))
         end
      end
   end

   structure EventLog = struct
      structure Type = struct
         open Word16Flags
         type t = flags
         val auditFailure = wc_EVENTLOG_AUDIT_FAILURE
         val auditSuccess = wc_EVENTLOG_AUDIT_SUCCESS
         val error        = wc_EVENTLOG_ERROR_TYPE
         val information  = wc_EVENTLOG_INFORMATION_TYPE
         val warning      = wc_EVENTLOG_WARNING_TYPE
      end
   end

   structure Module = struct
      type t = C.voidptr

      fun getFileName m = let
         val m' = getOpt (m, null)
      in
         onError0ElseTruncatedSize
            (fn () => F"Module.getFileName"[A (opt ptr) m]) 0w255
            (fn (b, s) => F_win_GetModuleFileName.f' (m', b, s))
      end
   end

   structure File = struct
      fun getShortName p =
          one (withZs p)
              (fn p' =>
                  onError0ElseRequiredSize
                     (fn () => F"Path.getShortName"[A str p])
                     (fn (b, s) => F_win_GetShortPathName.f' (p', b, s)))

      fun copy {from, to, failIfExists} =
          one (withZs from >>& withZs to)
              (fn from' & to' =>
                  raiseOnFalse
                     (fn () => F"File.copy"[A str from, A str to,
                                            A bool failIfExists])
                     F_win_CopyFile.f' (from', to', toCBool failIfExists))
   end

   structure Wait = struct
      type t = C.voidptr

      val hash =
          Word.xorb o 0wx55555555 <\ Misc.psdes o C.Cvt.ml_ulong o C.U.p2i
      val compare = C.Ptr.compare'

      datatype 'a result
        = ABANDONED of 'a
        | OBJECT of 'a
        | TIMEOUT

      val object    = wc_WAIT_OBJECT_0
      val abandoned = wc_WAIT_ABANDONED_0
      val timeout   = wc_WAIT_TIMEOUT
      val failed    = wc_WAIT_FAILED
      val infinite  = wc_INFINITE

      fun wait name all ws t = let
         val n = Word.fromInt (length ws)
         val s = C.S.voidptr
      in
         one (withAlloc (fn () => C.alloc' s n))
             (fn hs =>
                 (List.appi (fn (i, (w, _)) =>
                                C.Set.voidptr' (C.Ptr.sub' s (hs, i), w)) ws
                ; let val res =
                          F_win_WaitForMultipleObjects.f'
                             (n, C.Ptr.ro' hs, toCBool all,
                              case t of
                                 NONE => infinite
                               | SOME t =>
                                 Word.fromLargeInt (Time.toMilliseconds t))
                      fun get off = #2 (List.sub (ws, Word.toIntX (res - off)))
                  in
                     if res = timeout then
                        TIMEOUT
                     else if object <= res andalso res < object+n then
                        OBJECT (get object)
                     else if abandoned <= res andalso res < abandoned+n then
                        ABANDONED (get abandoned)
                     else if res = failed then
                        raiseLastError
                           (fn () => F name [A (lst ptr) (map #1 ws),
                                             A (opt time) t])
                     else
                        fail "Unsupported WaitForMultipleObjects functionality"
                  end))
      end

      fun any ? = wait "Wait.any" false ?
      fun all ? = wait "Wait.all" true ?
   end

   structure Semaphore = struct
      type t = C.voidptr
      fun create {init, max, name} =
          one (withOptZs name)
              (fn name' =>
                  raiseOnNull
                     (fn () => F"Semaphore.create"
                                [A int init, A int max, A (opt str) name])
                     F_win_CreateSemaphore.f' (null, init, max, name'))
      val close = ptrToBool "Semaphore.close" F_win_CloseHandle.f'
      fun release (s, n) =
          one withLong
              (fn result =>
                  (raiseOnFalse
                      (fn () => F"Semaphore.release"[A ptr s, A int n])
                      F_win_ReleaseSemaphore.f' (s, n, C.Ptr.|&! result)
                 ; C.Get.slong' result))
      val toWait = id
   end

   structure Mutex = struct
      type t = C.voidptr
      fun create {name, own} =
          one (withOptZs name)
              (fn name' =>
                  raiseOnNull
                     (fn () => F"Mutex.create"[A (opt str) name, A bool own])
                     F_win_CreateMutex.f' (null, toCBool own, name'))
      val close = ptrToBool "Mutex.close" F_win_CloseHandle.f'
      val release = ptrToBool "Mutex.release" F_win_ReleaseMutex.f'
      val toWait = id
   end

   structure Timer = struct
      type t = C.voidptr
      fun create {manual, name} =
          one (withOptZs name)
              (fn n' =>
                  raiseOnNull
                     (fn () => F"Timer.create"[A bool manual, A (opt str) name])
                     F_win_CreateWaitableTimer.f' (null, toCBool manual, n'))
      val close = ptrToBool "Timer.close" F_win_CloseHandle.f'
      fun mk name toDue {timer, due, period} = let
         val due' = toDue o Int64.fromLarge
                               |< LargeInt.quot (Time.toNanoseconds due, 100)
         val period' =
             case period of
                NONE => 0
              | SOME p => Int32.fromLarge (Time.toMilliseconds p)
      in
         raiseOnFalse
            (fn () => F name [A ptr timer, A time due, A (opt time) period])
            F_win_SetWaitableTimer.f' (timer, due', period', 0)
      end
      val setAbs = mk "Timer.setAbs" id
      val setRel = mk "Timer.setRel" op ~
      val cancel = ptrToBool "Timer.cancel" F_win_CancelWaitableTimer.f'
      val toWait = id
   end

   structure FileChange = struct
      structure Filter = struct
         open Word32Flags
         type t = flags
         val attributes = wc_FILE_NOTIFY_CHANGE_ATTRIBUTES
         val dirName    = wc_FILE_NOTIFY_CHANGE_DIR_NAME
         val fileName   = wc_FILE_NOTIFY_CHANGE_FILE_NAME
         val lastWrite  = wc_FILE_NOTIFY_CHANGE_LAST_WRITE
         val security   = wc_FILE_NOTIFY_CHANGE_SECURITY
         val size       = wc_FILE_NOTIFY_CHANGE_SIZE
      end

      type t = C.voidptr
      fun first (n, b, f) =
          one (withZs n)
              (fn n' =>
                  raiseOnNull
                     (fn () => F"FileChange.first"[A str n, A bool b, A w32 f])
                     F_win_FindFirstChangeNotification.f'
                     (n', toCBool b, f))
      val next = ptrToBool "FileChange.next" F_win_FindNextChangeNotification.f'
      val close = ptrToBool "FileChange.close" F_win_FindCloseChangeNotification.f'
      val toWait = id
   end

   structure Window = struct
      type t = C.voidptr

      fun find {class, window} =
          one (withOptZs class >>& withOptZs window)
              (fn c & w =>
                  raiseOnNullIfErrorElseNone
                     (fn () => F"Window.find"
                                [A (opt str) class, A (opt str) window])
                     F_win_FindWindow.f' (c, w))

      structure SW = struct
         type t = Int.t
         val forceminimize   = wc_SW_FORCEMINIMIZE
         val hide            = wc_SW_HIDE
         val maximize        = wc_SW_MAXIMIZE
         val minimize        = wc_SW_MINIMIZE
         val restore         = wc_SW_RESTORE
         val show            = wc_SW_SHOW
         val showdefault     = wc_SW_SHOWDEFAULT
         val showmaximized   = wc_SW_SHOWMAXIMIZED
         val showminimized   = wc_SW_SHOWMINIMIZED
         val showminnoactive = wc_SW_SHOWMINNOACTIVE
         val showna          = wc_SW_SHOWNA
         val shownoactivate  = wc_SW_SHOWNOACTIVATE
         val shownormal      = wc_SW_SHOWNORMAL
      end

      fun show (w, c) = 0 <> F_win_ShowWindow.f' (w, c)
   end
end
