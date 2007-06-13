/* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

/*
 * Specifications of Windows utilities.
 */

#ifndef WIN_H_20070205
#define WIN_H_20070205

/************************************************************************/

#ifndef C_CODE
# define C_CODE(x) x;
#endif

#ifndef WIN_TYPEDEF
# define WIN_TYPEDEF(name, type) C_CODE(typedef type name)
#endif

#ifndef WIN_CONST
# define WIN_CONST(name, type)
#endif

#ifndef WIN_FUNCTION
# define WIN_FUNCTION(name, result, arity, args) C_CODE(result win_##name args)
#endif

/************************************************************************/

WIN_TYPEDEF(LPVOID, void *)

WIN_TYPEDEF(BOOL, int)
WIN_TYPEDEF(BYTE, unsigned char)
WIN_TYPEDEF(WORD, unsigned short)
WIN_TYPEDEF(DWORD, unsigned long)
WIN_TYPEDEF(LONG, long)
WIN_TYPEDEF(LONGLONG, long long)

WIN_TYPEDEF(LPBYTE, BYTE *)
WIN_TYPEDEF(LPDWORD, DWORD *)
WIN_TYPEDEF(LPLONG, LONG *)

WIN_TYPEDEF(LPCTSTR, const char *)
WIN_TYPEDEF(LPTSTR, char *)

WIN_TYPEDEF(LPSECURITY_ATTRIBUTES, void *)
WIN_TYPEDEF(PFILETIME, void *)

/************************************************************************/

WIN_FUNCTION(GetLastError, DWORD, 0, (void))

WIN_CONST(ERROR_INSUFFICIENT_BUFFER, DWORD)
WIN_CONST(ERROR_MORE_DATA, DWORD)
WIN_CONST(ERROR_NO_MORE_ITEMS, DWORD)
WIN_CONST(ERROR_SUCCESS, DWORD)

/************************************************************************/

WIN_TYPEDEF(HMODULE, void *)

WIN_FUNCTION(GetModuleFileName, DWORD, 3, (HMODULE, LPTSTR, DWORD))

/************************************************************************/

WIN_FUNCTION(CopyFile, BOOL, 3, (LPCTSTR, LPCTSTR, BOOL))
WIN_FUNCTION(GetShortPathName, DWORD, 3, (LPCTSTR, LPTSTR, DWORD))

/************************************************************************/

WIN_TYPEDEF(HLOCAL, void *)

WIN_FUNCTION(LocalFree, HLOCAL, 1, (HLOCAL))

/************************************************************************/

WIN_TYPEDEF(HKEY, void *)

WIN_CONST(HKEY_CLASSES_ROOT, HKEY)
WIN_CONST(HKEY_CURRENT_CONFIG, HKEY)
WIN_CONST(HKEY_CURRENT_USER, HKEY)
WIN_CONST(HKEY_DYN_DATA, HKEY)
WIN_CONST(HKEY_LOCAL_MACHINE, HKEY)
WIN_CONST(HKEY_PERFORMANCE_DATA, HKEY)
WIN_CONST(HKEY_USERS, HKEY)

WIN_TYPEDEF(PHKEY, HKEY *)

WIN_TYPEDEF(REGSAM, unsigned long)

WIN_CONST(KEY_ALL_ACCESS, REGSAM)
WIN_CONST(KEY_CREATE_LINK, REGSAM)
WIN_CONST(KEY_CREATE_SUB_KEY, REGSAM)
WIN_CONST(KEY_ENUMERATE_SUB_KEYS, REGSAM)
WIN_CONST(KEY_EXECUTE, REGSAM)
WIN_CONST(KEY_NOTIFY, REGSAM)
WIN_CONST(KEY_QUERY_VALUE, REGSAM)
WIN_CONST(KEY_READ, REGSAM)
WIN_CONST(KEY_SET_VALUE, REGSAM)
WIN_CONST(KEY_WRITE, REGSAM)

WIN_CONST(REG_BINARY, DWORD)
WIN_CONST(REG_DWORD, DWORD)
WIN_CONST(REG_DWORD_LITTLE_ENDIAN, DWORD)
WIN_CONST(REG_DWORD_BIG_ENDIAN, DWORD)
WIN_CONST(REG_EXPAND_SZ, DWORD)
WIN_CONST(REG_LINK, DWORD)
WIN_CONST(REG_MULTI_SZ, DWORD)
WIN_CONST(REG_NONE, DWORD)
WIN_CONST(REG_QWORD, DWORD)
WIN_CONST(REG_QWORD_LITTLE_ENDIAN, DWORD)
WIN_CONST(REG_SZ, DWORD)

WIN_CONST(REG_CREATED_NEW_KEY, DWORD)
WIN_CONST(REG_OPENED_EXISTING_KEY, DWORD)

WIN_FUNCTION(RegCloseKey, LONG, 1, (HKEY))
WIN_FUNCTION(RegCreateKeyEx, LONG, 9,
             (HKEY, LPCTSTR, DWORD, LPTSTR, DWORD, REGSAM,
              LPSECURITY_ATTRIBUTES, PHKEY, LPDWORD))
WIN_FUNCTION(RegDeleteKey, LONG, 2, (HKEY, LPCTSTR))
WIN_FUNCTION(RegDeleteValue, LONG, 2, (HKEY, LPCTSTR))
WIN_FUNCTION(RegEnumKeyEx, LONG, 8,
             (HKEY, DWORD, LPTSTR, LPDWORD, LPDWORD, LPTSTR, LPDWORD,
              PFILETIME))
WIN_FUNCTION(RegEnumValue, LONG, 8,
             (HKEY, DWORD, LPTSTR, LPDWORD, LPDWORD, LPDWORD, LPBYTE, LPDWORD))
WIN_FUNCTION(RegOpenKeyEx, LONG, 5, (HKEY, LPCTSTR, DWORD, REGSAM, PHKEY))
WIN_FUNCTION(RegQueryValueEx, LONG, 6,
             (HKEY, LPCTSTR, LPDWORD, LPDWORD, LPBYTE, LPDWORD))
WIN_FUNCTION(RegSetValueEx, LONG, 6,
             (HKEY, LPCTSTR, DWORD, DWORD, const BYTE *, DWORD))

/************************************************************************/

WIN_TYPEDEF(HANDLE, void *)

WIN_FUNCTION(CloseHandle, BOOL, 1, (HANDLE))

/************************************************************************/

WIN_CONST(EVENTLOG_ERROR_TYPE, WORD)
WIN_CONST(EVENTLOG_AUDIT_FAILURE, WORD)
WIN_CONST(EVENTLOG_AUDIT_SUCCESS, WORD)
WIN_CONST(EVENTLOG_INFORMATION_TYPE, WORD)
WIN_CONST(EVENTLOG_WARNING_TYPE, WORD)

WIN_TYPEDEF(PSID, void *)

WIN_FUNCTION(RegisterEventSource, HANDLE, 2, (LPCTSTR, LPCTSTR))
WIN_FUNCTION(DeregisterEventSource, BOOL, 1, (HANDLE))
WIN_FUNCTION(ReportEvent, BOOL, 9,
             (HANDLE, WORD, WORD, DWORD, PSID, WORD, DWORD, LPCTSTR *, LPVOID))

/************************************************************************/

WIN_CONST(WAIT_OBJECT_0, DWORD)
WIN_CONST(WAIT_ABANDONED_0, DWORD)
WIN_CONST(WAIT_IO_COMPLETION, DWORD)
WIN_CONST(WAIT_TIMEOUT, DWORD)
WIN_CONST(WAIT_FAILED, DWORD)

WIN_CONST(INFINITE, DWORD)

WIN_FUNCTION(WaitForMultipleObjectsEx, DWORD, 5,
             (DWORD, const HANDLE *, BOOL, DWORD, BOOL))
WIN_FUNCTION(WaitForMultipleObjects, DWORD, 4,
             (DWORD, const HANDLE *, BOOL, DWORD))
WIN_FUNCTION(WaitForSingleObject, DWORD, 2, (HANDLE, DWORD))
WIN_FUNCTION(WaitForSingleObjectEx, DWORD, 3, (HANDLE, DWORD, BOOL))

/************************************************************************/

WIN_FUNCTION(CreateSemaphore, HANDLE, 4,
             (LPSECURITY_ATTRIBUTES, LONG, LONG, LPCTSTR))
WIN_FUNCTION(ReleaseSemaphore, BOOL, 3, (HANDLE, LONG, LPLONG))

/************************************************************************/

WIN_FUNCTION(CreateMutex, HANDLE, 3, (LPSECURITY_ATTRIBUTES, BOOL, LPCTSTR))
WIN_FUNCTION(ReleaseMutex, BOOL, 1, (HANDLE))

/************************************************************************/

WIN_FUNCTION(CreateWaitableTimer, HANDLE, 3,
             (LPSECURITY_ATTRIBUTES, BOOL, LPCTSTR))
WIN_FUNCTION(CancelWaitableTimer, BOOL, 1, (HANDLE))

C_CODE(BOOL win_SetWaitableTimer(HANDLE, LONGLONG, LONG, BOOL))

/************************************************************************/

WIN_CONST(FILE_NOTIFY_CHANGE_ATTRIBUTES, DWORD)
WIN_CONST(FILE_NOTIFY_CHANGE_DIR_NAME, DWORD)
WIN_CONST(FILE_NOTIFY_CHANGE_FILE_NAME, DWORD)
WIN_CONST(FILE_NOTIFY_CHANGE_LAST_WRITE, DWORD)
WIN_CONST(FILE_NOTIFY_CHANGE_SECURITY, DWORD)
WIN_CONST(FILE_NOTIFY_CHANGE_SIZE, DWORD)

WIN_FUNCTION(FindFirstChangeNotification, HANDLE, 3, (LPCTSTR, BOOL, DWORD))
WIN_FUNCTION(FindCloseChangeNotification, BOOL, 1, (HANDLE))
WIN_FUNCTION(FindNextChangeNotification, BOOL, 1, (HANDLE))

/************************************************************************/

WIN_FUNCTION(GetCurrentProcessId, DWORD, 0, (void))

/************************************************************************/

WIN_TYPEDEF(HWND, void *)

WIN_CONST(SW_FORCEMINIMIZE, int)
WIN_CONST(SW_HIDE, int)
WIN_CONST(SW_MAXIMIZE, int)
WIN_CONST(SW_MINIMIZE, int)
WIN_CONST(SW_RESTORE, int)
WIN_CONST(SW_SHOW, int)
WIN_CONST(SW_SHOWDEFAULT, int)
WIN_CONST(SW_SHOWMAXIMIZED, int)
WIN_CONST(SW_SHOWMINIMIZED, int)
WIN_CONST(SW_SHOWMINNOACTIVE, int)
WIN_CONST(SW_SHOWNA, int)
WIN_CONST(SW_SHOWNOACTIVATE, int)
WIN_CONST(SW_SHOWNORMAL, int)

WIN_FUNCTION(ShowWindow, BOOL, 2, (HWND, int))
WIN_FUNCTION(FindWindow, HWND, 2, (LPCTSTR, LPCTSTR))

/************************************************************************/

WIN_FUNCTION(FreeConsole, BOOL, 0, (void))

/************************************************************************/

WIN_FUNCTION(OutputDebugString, void, 1, (LPCTSTR))

/************************************************************************/

C_CODE(LPTSTR win_FormatErrorLocalAlloc(DWORD error))

C_CODE(LPSECURITY_ATTRIBUTES win_CreateAllAccessForWorldSA(void))

#endif
