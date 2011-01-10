module:    Dylan-user	
Synopsis:  Win32 API for non-GUI system services in "KERNEL32.DLL"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-kernel
  use functional-dylan;
  use C-FFI;
  use Win32-common;
  export Win32-kernel;
end;

define module Win32-kernel
  use functional-dylan;
  use C-FFI;
  use Win32-common,
    /* have to export here names that are used as structure accessors in
       both modules. */
    export: {X-value, X-value-setter, Y-value, Y-value-setter,
	       Left-value, Left-value-setter, Top-value, Top-value-setter,
	       Right-value, Right-value-setter,
	       Bottom-value, Bottom-value-setter,
	     offset-value, offset-value-setter,
	     cb-value, cb-value-setter, dwFlags-value, dwFlags-value-setter,
	     cbData-value, cbData-value-setter,
	     lpData-value, lpData-value-setter, 
	     dwSize-value, dwSize-value-setter,
	     wFlags-value, wFlags-value-setter,
	     u-value, u-value-setter, wSecond-value, wSecond-value-setter,
      /* also export some things that the user may want to use directly: */
	     %logand, <C-string>, <C-void*>, <HANDLE>, <HGLOBAL>, <HINSTANCE>,
	     <HLOCAL>, <HMODULE>, <HRSRC>, <LPSTR>, <LPTSTR>,
	     pointer-cast, with-stack-structure };


  // from "winbase.h":
  export $INVALID-HANDLE-VALUE, $INVALID-FILE-SIZE, $FILE-BEGIN,
	$FILE-CURRENT, $FILE-END, $TIME-ZONE-ID-INVALID, $WAIT-FAILED,
	$WAIT-TIMEOUT, $WAIT-IO-COMPLETION, $STILL-ACTIVE,
	$EXCEPTION-ACCESS-VIOLATION, $EXCEPTION-DATATYPE-MISALIGNMENT,
	$EXCEPTION-BREAKPOINT, $EXCEPTION-SINGLE-STEP,
	$EXCEPTION-ARRAY-BOUNDS-EXCEEDED, $EXCEPTION-FLT-DENORMAL-OPERAND,
	$EXCEPTION-FLT-DIVIDE-BY-ZERO, $EXCEPTION-FLT-INEXACT-RESULT,
	$EXCEPTION-FLT-INVALID-OPERATION, $EXCEPTION-FLT-OVERFLOW,
	$EXCEPTION-FLT-STACK-CHECK, $EXCEPTION-FLT-UNDERFLOW,
	$EXCEPTION-INT-DIVIDE-BY-ZERO, $EXCEPTION-INT-OVERFLOW,
	$EXCEPTION-PRIV-INSTRUCTION, $EXCEPTION-IN-PAGE-ERROR,
	$EXCEPTION-ILLEGAL-INSTRUCTION, $EXCEPTION-NONCONTINUABLE-EXCEPTION,
	$EXCEPTION-STACK-OVERFLOW, $EXCEPTION-INVALID-DISPOSITION,
	$EXCEPTION-GUARD-PAGE, $EXCEPTION-INVALID-HANDLE, $CONTROL-C-EXIT,
	$FILE-FLAG-WRITE-THROUGH, $FILE-FLAG-OVERLAPPED,
	$FILE-FLAG-NO-BUFFERING, $FILE-FLAG-RANDOM-ACCESS,
	$FILE-FLAG-SEQUENTIAL-SCAN, $FILE-FLAG-DELETE-ON-CLOSE,
	$FILE-FLAG-BACKUP-SEMANTICS, $FILE-FLAG-POSIX-SEMANTICS,
	$FILE-FLAG-OPEN-REPARSE-POINT, $FILE-FLAG-OPEN-NO-RECALL,
	$CREATE-NEW, $CREATE-ALWAYS, $OPEN-EXISTING, $OPEN-ALWAYS,
	$TRUNCATE-EXISTING;
  export $PIPE-ACCESS-INBOUND, $PIPE-ACCESS-OUTBOUND,
	$PIPE-ACCESS-DUPLEX;
  export $PIPE-CLIENT-END, $PIPE-SERVER-END;
  export $PIPE-WAIT, $PIPE-NOWAIT, $PIPE-READMODE-BYTE,
	$PIPE-READMODE-MESSAGE, $PIPE-TYPE-BYTE, $PIPE-TYPE-MESSAGE;
  export $PIPE-UNLIMITED-INSTANCES, $SECURITY-CONTEXT-TRACKING,
	$SECURITY-EFFECTIVE-ONLY, $SECURITY-SQOS-PRESENT,
	$SECURITY-VALID-SQOS-FLAGS;
  export Internal-value, Internal-value-setter, InternalHigh-value,
	InternalHigh-value-setter, OffsetHigh-value, OffsetHigh-value-setter,
	hEvent-value, hEvent-value-setter, <OVERLAPPED>, <LPOVERLAPPED>;
  export nLength-value, nLength-value-setter,
	lpSecurityDescriptor-value, lpSecurityDescriptor-value-setter,
	bInheritHandle-value, bInheritHandle-value-setter,
	<SECURITY-ATTRIBUTES>, <LPSECURITY-ATTRIBUTES>,
	<PSECURITY-ATTRIBUTES>;
  export hProcess-value, hProcess-value-setter, hThread-value,
	hThread-value-setter, dwProcessId-value, dwProcessId-value-setter,
	dwThreadId-value, dwThreadId-value-setter, <PROCESS-INFORMATION>,
	<LPPROCESS-INFORMATION>, <PPROCESS-INFORMATION>;
  export dwLowDateTime-value, dwLowDateTime-value-setter,
	dwHighDateTime-value, dwHighDateTime-value-setter, <FILETIME>,
	<LPFILETIME>, <PFILETIME>;
  export wYear-value, wYear-value-setter, wMonth-value,
	wMonth-value-setter, wDayOfWeek-value, wDayOfWeek-value-setter,
	wDay-value, wDay-value-setter, wHour-value, wHour-value-setter,
	wMinute-value, wMinute-value-setter, wMilliseconds-value,
	wMilliseconds-value-setter, <SYSTEMTIME>, <LPSYSTEMTIME>,
	<PSYSTEMTIME>;
  export $MUTEX-MODIFY-STATE, $MUTEX-ALL-ACCESS;
  export $SP-SERIALCOMM;
  export $PST-UNSPECIFIED, $PST-RS232, $PST-PARALLELPORT, $PST-RS422,
	$PST-RS423, $PST-RS449, $PST-MODEM, $PST-FAX, $PST-SCANNER,
	$PST-NETWORK-BRIDGE, $PST-LAT, $PST-TCPIP-TELNET, $PST-X25;
  export $PCF-DTRDSR, $PCF-RTSCTS, $PCF-RLSD, $PCF-PARITY-CHECK,
	$PCF-XONXOFF, $PCF-SETXCHAR, $PCF-TOTALTIMEOUTS, $PCF-INTTIMEOUTS,
	$PCF-SPECIALCHARS, $PCF-16BITMODE;
  export $SP-PARITY, $SP-BAUD, $SP-DATABITS, $SP-STOPBITS,
	$SP-HANDSHAKING, $SP-PARITY-CHECK, $SP-RLSD;
  export $BAUD-075, $BAUD-110, $BAUD-134-5, $BAUD-150, $BAUD-300,
	$BAUD-600, $BAUD-1200, $BAUD-1800, $BAUD-2400, $BAUD-4800,
	$BAUD-7200, $BAUD-9600, $BAUD-14400, $BAUD-19200, $BAUD-38400,
	$BAUD-56K, $BAUD-128K, $BAUD-115200, $BAUD-57600, $BAUD-USER;
  export $DATABITS-5, $DATABITS-6, $DATABITS-7, $DATABITS-8,
	$DATABITS-16, $DATABITS-16X;
  export $STOPBITS-10, $STOPBITS-15, $STOPBITS-20, $PARITY-NONE,
	$PARITY-ODD, $PARITY-EVEN, $PARITY-MARK, $PARITY-SPACE,
	wPacketLength-value, wPacketLength-value-setter,
	wPacketVersion-value, wPacketVersion-value-setter,
	dwServiceMask-value, dwServiceMask-value-setter, dwMaxTxQueue-value,
	dwMaxTxQueue-value-setter, dwMaxRxQueue-value,
	dwMaxRxQueue-value-setter, dwMaxBaud-value, dwMaxBaud-value-setter,
	dwProvSubType-value, dwProvSubType-value-setter,
	dwProvCapabilities-value, dwProvCapabilities-value-setter,
	dwSettableParams-value, dwSettableParams-value-setter,
	dwSettableBaud-value, dwSettableBaud-value-setter,
	wSettableData-value, wSettableData-value-setter,
	wSettableStopParity-value, wSettableStopParity-value-setter,
	dwCurrentTxQueue-value, dwCurrentTxQueue-value-setter,
	dwCurrentRxQueue-value, dwCurrentRxQueue-value-setter,
	dwProvSpec1-value, dwProvSpec1-value-setter, dwProvSpec2-value,
	dwProvSpec2-value-setter, wcProvChar-array, wcProvChar-array-setter,
	wcProvChar-value, <COMMPROP>, <LPCOMMPROP>, $COMMPROP-INITIALIZED,
	fCtsHold-value, fCtsHold-value-setter, fDsrHold-value,
	fDsrHold-value-setter, fRlsdHold-value, fRlsdHold-value-setter,
	fXoffHold-value, fXoffHold-value-setter, fXoffSent-value,
	fXoffSent-value-setter, fEof-value, fEof-value-setter, fTxim-value,
	fTxim-value-setter, cbInQue-value, cbInQue-value-setter,
	cbOutQue-value, cbOutQue-value-setter, <COMSTAT>, <LPCOMSTAT>;
  export $DTR-CONTROL-DISABLE, $DTR-CONTROL-ENABLE,
	$DTR-CONTROL-HANDSHAKE;
  export $RTS-CONTROL-DISABLE, $RTS-CONTROL-ENABLE,
	$RTS-CONTROL-HANDSHAKE, $RTS-CONTROL-TOGGLE, DCBlength-value,
	DCBlength-value-setter, BaudRate-value, BaudRate-value-setter,
	fBinary-value, fBinary-value-setter, fParity-value,
	fParity-value-setter, fOutxCtsFlow-value, fOutxCtsFlow-value-setter,
	fOutxDsrFlow-value, fOutxDsrFlow-value-setter, fDtrControl-value,
	fDtrControl-value-setter, fDsrSensitivity-value,
	fDsrSensitivity-value-setter, fTXContinueOnXoff-value,
	fTXContinueOnXoff-value-setter, fOutX-value, fOutX-value-setter,
	fInX-value, fInX-value-setter, fErrorChar-value,
	fErrorChar-value-setter, fNull-value, fNull-value-setter,
	fRtsControl-value, fRtsControl-value-setter, fAbortOnError-value,
	fAbortOnError-value-setter, fDummy2-value, fDummy2-value-setter,
	XonLim-value, XonLim-value-setter, XoffLim-value,
	XoffLim-value-setter, ByteSize-value, ByteSize-value-setter,
	Parity-value, Parity-value-setter, StopBits-value,
	StopBits-value-setter, XonChar-value, XonChar-value-setter,
	XoffChar-value, XoffChar-value-setter, ErrorChar-value,
	ErrorChar-value-setter, EofChar-value, EofChar-value-setter,
	EvtChar-value, EvtChar-value-setter, <DCB>, <LPDCB>;
  export ReadIntervalTimeout-value, ReadIntervalTimeout-value-setter,
	ReadTotalTimeoutMultiplier-value,
	ReadTotalTimeoutMultiplier-value-setter,
	ReadTotalTimeoutConstant-value,
	ReadTotalTimeoutConstant-value-setter,
	WriteTotalTimeoutMultiplier-value,
	WriteTotalTimeoutMultiplier-value-setter,
	WriteTotalTimeoutConstant-value,
	WriteTotalTimeoutConstant-value-setter, <COMMTIMEOUTS>,
	<LPCOMMTIMEOUTS>;
  export wVersion-value, wVersion-value-setter, dcb-value,
	dcb-value-setter, dwProviderSubType-value,
	dwProviderSubType-value-setter, dwProviderOffset-value,
	dwProviderOffset-value-setter, dwProviderSize-value,
	dwProviderSize-value-setter, wcProviderData-array,
	wcProviderData-array-setter, wcProviderData-value, <COMMCONFIG>,
	<LPCOMMCONFIG>;
  export wProcessorArchitecture-value,
	wProcessorArchitecture-value-setter, dwPageSize-value,
	dwPageSize-value-setter, lpMinimumApplicationAddress-value,
	lpMinimumApplicationAddress-value-setter,
	lpMaximumApplicationAddress-value,
	lpMaximumApplicationAddress-value-setter,
	dwActiveProcessorMask-value, dwActiveProcessorMask-value-setter,
	dwNumberOfProcessors-value, dwNumberOfProcessors-value-setter,
	dwProcessorType-value, dwProcessorType-value-setter,
	dwAllocationGranularity-value, dwAllocationGranularity-value-setter,
	wProcessorLevel-value, wProcessorLevel-value-setter,
	wProcessorRevision-value, wProcessorRevision-value-setter,
	<SYSTEM-INFO>, <LPSYSTEM-INFO>, $GMEM-FIXED, $GMEM-MOVEABLE,
	$GMEM-NOCOMPACT, $GMEM-NODISCARD, $GMEM-ZEROINIT, $GMEM-MODIFY,
	$GMEM-DISCARDABLE, $GMEM-NOT-BANKED, $GMEM-SHARE, $GMEM-DDESHARE,
	$GMEM-NOTIFY, $GMEM-LOWER, $GMEM-VALID-FLAGS, $GMEM-INVALID-HANDLE,
	$GHND, $GPTR, $GMEM-DISCARDED, $GMEM-LOCKCOUNT, dwLength-value,
	dwLength-value-setter, dwMemoryLoad-value, dwMemoryLoad-value-setter,
	dwTotalPhys-value, dwTotalPhys-value-setter, dwAvailPhys-value,
	dwAvailPhys-value-setter, dwTotalPageFile-value,
	dwTotalPageFile-value-setter, dwAvailPageFile-value,
	dwAvailPageFile-value-setter, dwTotalVirtual-value,
	dwTotalVirtual-value-setter, dwAvailVirtual-value,
	dwAvailVirtual-value-setter, <MEMORYSTATUS>, <LPMEMORYSTATUS>,
	$LMEM-FIXED, $LMEM-MOVEABLE, $LMEM-NOCOMPACT, $LMEM-NODISCARD,
	$LMEM-ZEROINIT, $LMEM-MODIFY, $LMEM-DISCARDABLE, $LMEM-VALID-FLAGS,
	$LMEM-INVALID-HANDLE, $LHND, $LPTR, $NONZEROLHND, $NONZEROLPTR,
	$LMEM-DISCARDED, $LMEM-LOCKCOUNT;
  export $CREATE-SUSPENDED, $DETACHED-PROCESS, $CREATE-NEW-CONSOLE,
	$NORMAL-PRIORITY-CLASS, $IDLE-PRIORITY-CLASS, $HIGH-PRIORITY-CLASS,
	$REALTIME-PRIORITY-CLASS, $CREATE-NEW-PROCESS-GROUP,
	$CREATE-UNICODE-ENVIRONMENT, $CREATE-SEPARATE-WOW-VDM,
	$CREATE-SHARED-WOW-VDM, $CREATE-FORCEDOS, $CREATE-DEFAULT-ERROR-MODE,
	$CREATE-NO-WINDOW, $DRIVE-UNKNOWN, $DRIVE-NO-ROOT-DIR,
	$DRIVE-REMOVABLE, $DRIVE-FIXED, $DRIVE-REMOTE, $DRIVE-CDROM,
	$DRIVE-RAMDISK;
  export $FILE-TYPE-UNKNOWN, $FILE-TYPE-DISK, $FILE-TYPE-CHAR,
	$FILE-TYPE-PIPE, $FILE-TYPE-REMOTE, $STD-INPUT-HANDLE,
	$STD-OUTPUT-HANDLE, $STD-ERROR-HANDLE, $NOPARITY, $ODDPARITY,
	$EVENPARITY, $MARKPARITY, $SPACEPARITY, $ONESTOPBIT, $ONE5STOPBITS,
	$TWOSTOPBITS, $IGNORE, $INFINITE;
  export $CBR-110, $CBR-300, $CBR-600, $CBR-1200, $CBR-2400,
	$CBR-4800, $CBR-9600, $CBR-14400, $CBR-19200, $CBR-38400, $CBR-56000,
	$CBR-57600, $CBR-115200, $CBR-128000, $CBR-256000;
  export $CE-RXOVER, $CE-OVERRUN, $CE-RXPARITY, $CE-FRAME, $CE-BREAK,
	$CE-TXFULL, $CE-PTO, $CE-IOE, $CE-DNS, $CE-OOP, $CE-MODE, $IE-BADID,
	$IE-OPEN, $IE-NOPEN, $IE-MEMORY, $IE-DEFAULT, $IE-HARDWARE,
	$IE-BYTESIZE, $IE-BAUDRATE;
  export $EV-RXCHAR, $EV-RXFLAG, $EV-TXEMPTY, $EV-CTS, $EV-DSR,
	$EV-RLSD, $EV-BREAK, $EV-ERR, $EV-RING, $EV-PERR, $EV-RX80FULL,
	$EV-EVENT1, $EV-EVENT2;
  export $SETXOFF, $SETXON, $SETRTS, $CLRRTS, $SETDTR, $CLRDTR,
	$RESETDEV, $SETBREAK, $CLRBREAK;
  export $PURGE-TXABORT, $PURGE-RXABORT, $PURGE-TXCLEAR,
	$PURGE-RXCLEAR, $LPTx;
  export $MS-CTS-ON, $MS-DSR-ON, $MS-RING-ON, $MS-RLSD-ON;
  export $S-QUEUEEMPTY, $S-THRESHOLD, $S-ALLTHRESHOLD;
  export $S-NORMAL, $S-LEGATO, $S-STACCATO;
  export $S-PERIOD512, $S-PERIOD1024, $S-PERIOD2048, $S-PERIODVOICE,
	$S-WHITE512, $S-WHITE1024, $S-WHITE2048, $S-WHITEVOICE, $S-SERDVNA,
	$S-SEROFM, $S-SERMACT, $S-SERQFUL, $S-SERBDNT, $S-SERDLN, $S-SERDCC,
	$S-SERDTP, $S-SERDVL, $S-SERDMD, $S-SERDSH, $S-SERDPT, $S-SERDFQ,
	$S-SERDDR, $S-SERDSR, $S-SERDST, $NMPWAIT-WAIT-FOREVER,
	$NMPWAIT-NOWAIT, $NMPWAIT-USE-DEFAULT-WAIT, $FS-CASE-IS-PRESERVED,
	$FS-CASE-SENSITIVE, $FS-UNICODE-STORED-ON-DISK, $FS-PERSISTENT-ACLS,
	$FS-VOL-IS-COMPRESSED, $FS-FILE-COMPRESSION;
  export $FILE-MAP-COPY, $FILE-MAP-WRITE, $FILE-MAP-READ,
	$FILE-MAP-ALL-ACCESS, $OF-READ, $OF-WRITE, $OF-READWRITE,
	$OF-SHARE-COMPAT, $OF-SHARE-EXCLUSIVE, $OF-SHARE-DENY-WRITE,
	$OF-SHARE-DENY-READ, $OF-SHARE-DENY-NONE, $OF-PARSE, $OF-DELETE,
	$OF-VERIFY, $OF-CANCEL, $OF-CREATE, $OF-PROMPT, $OF-EXIST,
	$OF-REOPEN, $OFS-MAXPATHNAME, cBytes-value, cBytes-value-setter,
	fFixedDisk-value, fFixedDisk-value-setter, nErrCode-value,
	nErrCode-value-setter, szPathName-array, szPathName-array-setter,
	szPathName-value, <OFSTRUCT>, <LPOFSTRUCT>, <POFSTRUCT>,
	FreeResource, LockResource, $MAXINTATOM, $INVALID-ATOM, FreeLibrary;
  export GetProcAddress, GetVersion, GlobalAlloc, GlobalReAlloc,
	GlobalSize, GlobalFlags;
  export GlobalLock, GlobalHandle;
  export GlobalUnlock;
  export GlobalFree, GlobalMemoryStatus, LocalAlloc, LocalReAlloc,
	LocalLock, LocalHandle, LocalUnlock, LocalSize, LocalFlags,
	LocalFree, FlushInstructionCache, VirtualAlloc, VirtualFree,
	VirtualProtect, VirtualProtectEx, HeapCreate, HeapDestroy;
  export HeapAlloc, HeapReAlloc, HeapFree, HeapSize, GetProcessHeap,
	GetProcessHeaps;
  export $SCS-32BIT-BINARY, $SCS-DOS-BINARY, $SCS-WOW-BINARY,
	$SCS-PIF-BINARY, $SCS-POSIX-BINARY, $SCS-OS216-BINARY, GetBinaryType,
	GetShortPathName, GetProcessAffinityMask;
  export GetProcessTimes, GetProcessWorkingSetSize,
	SetProcessWorkingSetSize, OpenProcess, GetCurrentProcess,
	GetCurrentProcessId, ExitProcess, TerminateProcess,
	GetExitCodeProcess;
  export FatalExit, GetEnvironmentStrings, FreeEnvironmentStrings,
	RaiseException;
  export <PTOP-LEVEL-EXCEPTION-FILTER>,
	<LPTOP-LEVEL-EXCEPTION-FILTER>, SetUnhandledExceptionFilter,
	GetLastError, SetLastError, GetOverlappedResult,
	CreateIoCompletionPort, GetQueuedCompletionStatus,
	PostQueuedCompletionStatus, $SEM-FAILCRITICALERRORS,
	$SEM-NOGPFAULTERRORBOX, $SEM-NOALIGNMENTFAULTEXCEPT,
	$SEM-NOOPENFILEERRORBOX, SetErrorMode, ReadProcessMemory,
	WriteProcessMemory;
  export DebugBreak, SetEvent, ResetEvent, PulseEvent,
	ReleaseSemaphore, ReleaseMutex, WaitForSingleObject,
	WaitForMultipleObjects, Sleep, LoadResource, SizeofResource;
  export GlobalDeleteAtom, InitAtomTable, DeleteAtom, SetHandleCount,
	GetLogicalDrives, LockFile, UnlockFile, LockFileEx,
	$LOCKFILE-FAIL-IMMEDIATELY, $LOCKFILE-EXCLUSIVE-LOCK, UnlockFileEx;
  export dwFileAttributes-value, dwFileAttributes-value-setter,
	ftCreationTime-value, ftCreationTime-value-setter,
	ftLastAccessTime-value, ftLastAccessTime-value-setter,
	ftLastWriteTime-value, ftLastWriteTime-value-setter,
	dwVolumeSerialNumber-value, dwVolumeSerialNumber-value-setter,
	nFileSizeHigh-value, nFileSizeHigh-value-setter, nFileSizeLow-value,
	nFileSizeLow-value-setter, nNumberOfLinks-value,
	nNumberOfLinks-value-setter, nFileIndexHigh-value,
	nFileIndexHigh-value-setter, nFileIndexLow-value,
	nFileIndexLow-value-setter, <BY-HANDLE-FILE-INFORMATION>,
	<LPBY-HANDLE-FILE-INFORMATION>, <PBY-HANDLE-FILE-INFORMATION>,
	GetFileInformationByHandle, GetFileType, GetFileSize, GetStdHandle,
	SetStdHandle, WriteFile, ReadFile, FlushFileBuffers, DeviceIoControl,
	SetEndOfFile, SetFilePointer, FindClose, GetFileTime, SetFileTime,
	CloseHandle, DuplicateHandle, GetHandleInformation,
	SetHandleInformation, $HANDLE-FLAG-INHERIT,
	$HANDLE-FLAG-PROTECT-FROM-CLOSE, $HINSTANCE-ERROR, LoadModule,
	WinExec, ClearCommBreak, ClearCommError, SetupComm,
	EscapeCommFunction, GetCommConfig, GetCommMask, GetCommProperties,
	GetCommModemStatus, GetCommState, GetCommTimeouts, PurgeComm,
	SetCommBreak, SetCommConfig, SetCommMask, SetCommState,
	SetCommTimeouts, TransmitCommChar, WaitCommEvent;
  export Beep, MulDiv, GetSystemTime, GetSystemTimeAsFileTime,
	SetSystemTime, GetLocalTime, SetLocalTime, GetSystemInfo;
  export Bias-value, Bias-value-setter, StandardName-array,
	StandardName-array-setter, StandardName-value, StandardDate-value,
	StandardDate-value-setter, StandardBias-value,
	StandardBias-value-setter, DaylightName-array,
	DaylightName-array-setter, DaylightName-value, DaylightDate-value,
	DaylightDate-value-setter, DaylightBias-value,
	DaylightBias-value-setter, <TIME-ZONE-INFORMATION>,
	<LPTIME-ZONE-INFORMATION>, <PTIME-ZONE-INFORMATION>,
	SystemTimeToTzSpecificLocalTime, GetTimeZoneInformation,
	SetTimeZoneInformation;
  export SystemTimeToFileTime, FileTimeToLocalFileTime,
	LocalFileTimeToFileTime, FileTimeToSystemTime, CompareFileTime,
	FileTimeToDosDateTime, DosDateTimeToFileTime, GetTickCount,
	SetSystemTimeAdjustment, GetSystemTimeAdjustment,
	$FORMAT-MESSAGE-ALLOCATE-BUFFER, $FORMAT-MESSAGE-IGNORE-INSERTS,
	$FORMAT-MESSAGE-FROM-STRING, $FORMAT-MESSAGE-FROM-HMODULE,
	$FORMAT-MESSAGE-FROM-SYSTEM, $FORMAT-MESSAGE-ARGUMENT-ARRAY,
	$FORMAT-MESSAGE-MAX-WIDTH-MASK, CreatePipe, ConnectNamedPipe,
	DisconnectNamedPipe, SetNamedPipeHandleState, GetNamedPipeInfo,
	PeekNamedPipe, TransactNamedPipe, CreateMailslot, GetMailslotInfo,
	SetMailslotInfo, MapViewOfFile;
  export FlushViewOfFile, UnmapViewOfFile;
  export $EFS-USE-RECOVERY-KEYS, <PFE-EXPORT-FUNC>;
  export <PFE-IMPORT-FUNC>;
  export lstrcmp, lstrcmpi, lstrcpyn, lstrcpy, lstrcat, lstrlen,
	IsTextUnicode, TlsAlloc, $TLS-OUT-OF-INDEXES, TlsGetValue,
	TlsSetValue, TlsFree;
  export <LPOVERLAPPED-COMPLETION-ROUTINE>, SleepEx,
	WaitForSingleObjectEx, WaitForMultipleObjectsEx, ReadFileEx,
	WriteFileEx;
  export $BACKUP-DATA;
  export $STARTF-USESHOWWINDOW, $STARTF-USESIZE, $STARTF-USEPOSITION,
	$STARTF-USECOUNTCHARS, $STARTF-USEFILLATTRIBUTE,
	$STARTF-RUNFULLSCREEN, $STARTF-FORCEONFEEDBACK,
	$STARTF-FORCEOFFFEEDBACK, $STARTF-USESTDHANDLES, $STARTF-USEHOTKEY;
  export lpDesktop-value, lpDesktop-value-setter, lpTitle-value,
	lpTitle-value-setter, dwX-value, dwX-value-setter, dwY-value,
	dwY-value-setter, dwXSize-value, dwXSize-value-setter, dwYSize-value,
	dwYSize-value-setter, dwXCountChars-value,
	dwXCountChars-value-setter, dwYCountChars-value,
	dwYCountChars-value-setter, dwFillAttribute-value,
	dwFillAttribute-value-setter, wShowWindow-value,
	wShowWindow-value-setter, hStdInput-value, hStdInput-value-setter,
	hStdOutput-value, hStdOutput-value-setter, hStdError-value,
	hStdError-value-setter, <STARTUPINFOA>, <LPSTARTUPINFOA>,
	<STARTUPINFO>, <LPSTARTUPINFO>, $SHUTDOWN-NORETRY,
	dwFileAttributes-value, dwFileAttributes-value-setter,
	ftCreationTime-value, ftCreationTime-value-setter,
	ftLastAccessTime-value, ftLastAccessTime-value-setter,
	ftLastWriteTime-value, ftLastWriteTime-value-setter,
	nFileSizeHigh-value, nFileSizeHigh-value-setter, nFileSizeLow-value,
	nFileSizeLow-value-setter, cFileName-array, cFileName-array-setter,
	cFileName-value, cAlternateFileName-array,
	cAlternateFileName-array-setter, cAlternateFileName-value,
	<WIN32-FIND-DATAA>, <LPWIN32-FIND-DATAA>, <PWIN32-FIND-DATAA>,
	<WIN32-FIND-DATA>, <PWIN32-FIND-DATA>, <LPWIN32-FIND-DATA>,
	dwFileAttributes-value, dwFileAttributes-value-setter,
	ftCreationTime-value, ftCreationTime-value-setter,
	ftLastAccessTime-value, ftLastAccessTime-value-setter,
	ftLastWriteTime-value, ftLastWriteTime-value-setter,
	nFileSizeHigh-value, nFileSizeHigh-value-setter, nFileSizeLow-value,
	nFileSizeLow-value-setter, <WIN32-FILE-ATTRIBUTE-DATA>,
	<LPWIN32-FILE-ATTRIBUTE-DATA>, CreateMutex, OpenMutex, CreateEvent,
	OpenEvent, CreateSemaphore, OpenSemaphore, CreateFileMapping,
	GetLogicalDriveStrings, LoadLibrary, LoadLibraryEx,
	$DONT-RESOLVE-DLL-REFERENCES, $LOAD-LIBRARY-AS-DATAFILE,
	$LOAD-WITH-ALTERED-SEARCH-PATH, GetModuleFileName, GetModuleHandle,
	CreateProcess, SetProcessShutdownParameters,
	GetProcessShutdownParameters, GetProcessVersion, FatalAppExit,
	GetStartupInfo, GetCommandLine, GetEnvironmentVariable,
	SetEnvironmentVariable, ExpandEnvironmentStrings, OutputDebugString,
	FindResource, FindResourceEx, <ENUMRESTYPEPROC>,
	<ENUMRESTYPEPROC>-callback-wrapper, <ENUMRESNAMEPROC>,
	<ENUMRESNAMEPROC>-callback-wrapper, <ENUMRESLANGPROC>,
	<ENUMRESLANGPROC>-callback-wrapper, EnumResourceTypes,
	EnumResourceNames, EnumResourceLanguages, UpdateResource,
	EndUpdateResource, GlobalAddAtom, GlobalFindAtom, GlobalGetAtomName,
	AddAtom, FindAtom, GetAtomName, GetDriveType, GetSystemDirectory,
	GetTempPath, GetTempFileName, GetWindowsDirectory,
	SetCurrentDirectory, GetCurrentDirectory, GetDiskFreeSpace,
	CreateDirectory, CreateDirectoryEx, RemoveDirectory, GetFullPathName,
	$DDD-RAW-TARGET-PATH, $DDD-REMOVE-DEFINITION,
	$DDD-EXACT-MATCH-ON-REMOVE, $DDD-NO-BROADCAST-SYSTEM,
	DefineDosDevice, QueryDosDevice, CreateFile, SetFileAttributes,
	GetFileAttributes, $GetFileExInfoStandard, $GetFileExMaxInfoLevel,
	GetCompressedFileSize, DeleteFile, FindFirstFile, FindNextFile,
	SearchPath, CopyFile, MoveFile, MoveFileEx,
	$MOVEFILE-REPLACE-EXISTING, $MOVEFILE-COPY-ALLOWED,
	$MOVEFILE-DELAY-UNTIL-REBOOT, $MOVEFILE-WRITE-THROUGH,
	CreateNamedPipe, GetNamedPipeHandleState, CallNamedPipe,
	WaitNamedPipe, SetVolumeLabel, SetFileApisToOEM, SetFileApisToANSI,
	AreFileApisANSI, GetVolumeInformation;
  export ClearEventLog, BackupEventLog, CloseEventLog,
	DeregisterEventSource, NotifyChangeEventLog,
	GetNumberOfEventLogRecords, GetOldestEventLogRecord, OpenEventLog,
	RegisterEventSource, OpenBackupEventLog, ReadEventLog;
  export FindFirstChangeNotification, FindNextChangeNotification,
	FindCloseChangeNotification, VirtualLock, VirtualUnlock,
	MapViewOfFileEx, SetPriorityClass, GetPriorityClass, IsBadReadPtr,
	IsBadWritePtr, IsBadHugeReadPtr, IsBadHugeWritePtr, IsBadCodePtr,
	IsBadStringPtr, BuildCommDCB, BuildCommDCBAndTimeouts,
	CommConfigDialog, GetDefaultCommConfig, SetDefaultCommConfig,
	$MAX-COMPUTERNAME-LENGTH, GetComputerName, SetComputerName,
	GetUserName;
  export $LOGON32-LOGON-INTERACTIVE, $LOGON32-LOGON-NETWORK,
	$LOGON32-LOGON-BATCH, $LOGON32-LOGON-SERVICE,
	$LOGON32-PROVIDER-DEFAULT, $LOGON32-PROVIDER-WINNT35;
  export QueryPerformanceCounter, QueryPerformanceFrequency;
  export dwOSVersionInfoSize-value, dwOSVersionInfoSize-value-setter,
	dwMajorVersion-value, dwMajorVersion-value-setter,
	dwMinorVersion-value, dwMinorVersion-value-setter,
	dwBuildNumber-value, dwBuildNumber-value-setter, dwPlatformId-value,
	dwPlatformId-value-setter, szCSDVersion-array,
	szCSDVersion-array-setter, szCSDVersion-value, <OSVERSIONINFOA>,
	<LPOSVERSIONINFOA>, <POSVERSIONINFOA>, <OSVERSIONINFO>,
	<POSVERSIONINFO>, <LPOSVERSIONINFO>, dwOSVersionInfoSize-value,
	dwOSVersionInfoSize-value-setter, dwMajorVersion-value,
	dwMajorVersion-value-setter, dwMinorVersion-value,
	dwMinorVersion-value-setter, dwBuildNumber-value,
	dwBuildNumber-value-setter, dwPlatformId-value,
	dwPlatformId-value-setter, szCSDVersion-array,
	szCSDVersion-array-setter, szCSDVersion-value,
	wServicePackMajor-value, wServicePackMajor-value-setter,
	wServicePackMinor-value, wServicePackMinor-value-setter,
	wReserved-array, wReserved-array-setter, wReserved-value,
	<OSVERSIONINFOEXA>, <LPOSVERSIONINFOEXA>, <POSVERSIONINFOEXA>,
	<OSVERSIONINFOEX>, <POSVERSIONINFOEX>, <LPOSVERSIONINFOEX>;
  export $VER-PLATFORM-WIN32s, $VER-PLATFORM-WIN32-WINDOWS,
	$VER-PLATFORM-WIN32-NT, GetVersionEx, $TC-NORMAL, $TC-HARDERR,
	$TC-GP-TRAP, $TC-SIGNAL;

  // Additional definitions from "winnt.h":

  // from "winnt.h":
  export $STATUS-WAIT-0, $STATUS-ABANDONED-WAIT-0, $STATUS-USER-APC,
	$STATUS-TIMEOUT, $STATUS-PENDING, $STATUS-SEGMENT-NOTIFICATION,
	$STATUS-GUARD-PAGE-VIOLATION, $STATUS-DATATYPE-MISALIGNMENT,
	$STATUS-BREAKPOINT, $STATUS-SINGLE-STEP, $STATUS-ACCESS-VIOLATION,
	$STATUS-IN-PAGE-ERROR, $STATUS-INVALID-HANDLE, $STATUS-NO-MEMORY,
	$STATUS-ILLEGAL-INSTRUCTION, $STATUS-NONCONTINUABLE-EXCEPTION,
	$STATUS-INVALID-DISPOSITION, $STATUS-ARRAY-BOUNDS-EXCEEDED,
	$STATUS-FLOAT-DENORMAL-OPERAND, $STATUS-FLOAT-DIVIDE-BY-ZERO,
	$STATUS-FLOAT-INEXACT-RESULT, $STATUS-FLOAT-INVALID-OPERATION,
	$STATUS-FLOAT-OVERFLOW, $STATUS-FLOAT-STACK-CHECK,
	$STATUS-FLOAT-UNDERFLOW, $STATUS-INTEGER-DIVIDE-BY-ZERO,
	$STATUS-INTEGER-OVERFLOW, $STATUS-PRIVILEGED-INSTRUCTION,
	$STATUS-STACK-OVERFLOW, $STATUS-CONTROL-C-EXIT;
  export $READ-CONTROL, $SYNCHRONIZE, $STANDARD-RIGHTS-REQUIRED,
	$STANDARD-RIGHTS-READ, $STANDARD-RIGHTS-WRITE,
	$STANDARD-RIGHTS-EXECUTE, $STANDARD-RIGHTS-ALL;
  export $GENERIC-READ, $GENERIC-WRITE;
  export $MUTANT-QUERY-STATE, $MUTANT-ALL-ACCESS;
  export $SECTION-QUERY, $SECTION-MAP-WRITE, $SECTION-MAP-READ,
	$SECTION-MAP-EXECUTE, $SECTION-EXTEND-SIZE, $SECTION-ALL-ACCESS;
  export $FILE-READ-DATA, $FILE-LIST-DIRECTORY, $FILE-WRITE-DATA,
	$FILE-ADD-FILE, $FILE-APPEND-DATA, $FILE-ADD-SUBDIRECTORY,
	$FILE-CREATE-PIPE-INSTANCE, $FILE-READ-EA, $FILE-WRITE-EA,
	$FILE-EXECUTE, $FILE-TRAVERSE, $FILE-DELETE-CHILD,
	$FILE-READ-ATTRIBUTES, $FILE-WRITE-ATTRIBUTES, $FILE-ALL-ACCESS,
	$FILE-GENERIC-READ, $FILE-GENERIC-WRITE, $FILE-GENERIC-EXECUTE,
	$FILE-SHARE-READ, $FILE-SHARE-WRITE, $FILE-SHARE-DELETE,
	$FILE-ATTRIBUTE-READONLY, $FILE-ATTRIBUTE-HIDDEN,
	$FILE-ATTRIBUTE-SYSTEM, $FILE-ATTRIBUTE-DIRECTORY,
	$FILE-ATTRIBUTE-ARCHIVE, $FILE-ATTRIBUTE-ENCRYPTED,
	$FILE-ATTRIBUTE-NORMAL, $FILE-ATTRIBUTE-TEMPORARY,
	$FILE-ATTRIBUTE-SPARSE-FILE, $FILE-ATTRIBUTE-REPARSE-POINT,
	$FILE-ATTRIBUTE-COMPRESSED, $FILE-ATTRIBUTE-OFFLINE,
	$FILE-NOTIFY-CHANGE-FILE-NAME, $FILE-NOTIFY-CHANGE-DIR-NAME,
	$FILE-NOTIFY-CHANGE-ATTRIBUTES, $FILE-NOTIFY-CHANGE-SIZE,
	$FILE-NOTIFY-CHANGE-LAST-WRITE, $FILE-NOTIFY-CHANGE-LAST-ACCESS,
	$FILE-NOTIFY-CHANGE-CREATION, $FILE-NOTIFY-CHANGE-SECURITY,
	$FILE-ACTION-ADDED, $FILE-ACTION-REMOVED, $FILE-ACTION-MODIFIED,
	$FILE-ACTION-RENAMED-OLD-NAME, $FILE-ACTION-RENAMED-NEW-NAME,
	$FILE-CASE-SENSITIVE-SEARCH, $FILE-CASE-PRESERVED-NAMES,
	$FILE-UNICODE-ON-DISK, $FILE-PERSISTENT-ACLS, $FILE-FILE-COMPRESSION,
	$FILE-VOLUME-QUOTAS, $FILE-SUPPORTS-SPARSE-FILES,
	$FILE-SUPPORTS-REPARSE-POINTS, $FILE-SUPPORTS-REMOTE-STORAGE,
	$FILE-VOLUME-IS-COMPRESSED, $FILE-SUPPORTS-OBJECT-IDS,
	$FILE-SUPPORTS-ENCRYPTION;
  export $ES-CONTINUOUS;
  export $LT-DONT-CARE, $LT-LOWEST-LATENCY;


  // from "winnls.h":
  export $MAX-LEADBYTES, $MAX-DEFAULTCHAR;
  export $MB-PRECOMPOSED, $MB-COMPOSITE, $MB-USEGLYPHCHARS,
	$MB-ERR-INVALID-CHARS, $WC-COMPOSITECHECK, $WC-DISCARDNS,
	$WC-SEPCHARS, $WC-DEFAULTCHAR;
  export $CT-CTYPE1, $CT-CTYPE2, $CT-CTYPE3;
  export $C1-UPPER, $C1-LOWER, $C1-DIGIT, $C1-SPACE, $C1-PUNCT,
	$C1-CNTRL, $C1-BLANK, $C1-XDIGIT, $C1-ALPHA;
  export $C2-LEFTTORIGHT, $C2-RIGHTTOLEFT, $C2-EUROPENUMBER,
	$C2-EUROPESEPARATOR, $C2-EUROPETERMINATOR, $C2-ARABICNUMBER,
	$C2-COMMONSEPARATOR, $C2-BLOCKSEPARATOR, $C2-SEGMENTSEPARATOR,
	$C2-WHITESPACE, $C2-OTHERNEUTRAL, $C2-NOTAPPLICABLE;
  export $C3-NONSPACING, $C3-DIACRITIC, $C3-VOWELMARK, $C3-SYMBOL,
	$C3-KATAKANA, $C3-HIRAGANA, $C3-HALFWIDTH, $C3-FULLWIDTH,
	$C3-IDEOGRAPH, $C3-KASHIDA, $C3-LEXICAL, $C3-ALPHA,
	$C3-NOTAPPLICABLE;
  export $NORM-IGNORECASE, $NORM-IGNORENONSPACE, $NORM-IGNORESYMBOLS,
	$NORM-IGNOREKANATYPE, $NORM-IGNOREWIDTH;
  export $MAP-FOLDCZONE, $MAP-PRECOMPOSED, $MAP-COMPOSITE,
	$MAP-FOLDDIGITS;
  export $LCMAP-LOWERCASE, $LCMAP-UPPERCASE, $LCMAP-SORTKEY,
	$LCMAP-BYTEREV, $LCMAP-HIRAGANA, $LCMAP-KATAKANA, $LCMAP-HALFWIDTH,
	$LCMAP-FULLWIDTH, $LCMAP-LINGUISTIC-CASING,
	$LCMAP-SIMPLIFIED-CHINESE, $LCMAP-TRADITIONAL-CHINESE;
  export $LCID-INSTALLED, $LCID-SUPPORTED, $LCID-ALTERNATE-SORTS;
  export $CP-INSTALLED, $CP-SUPPORTED;
  export $SORT-STRINGSORT;
  export $CSTR-LESS-THAN, $CSTR-EQUAL, $CSTR-GREATER-THAN;
  export $CP-ACP, $CP-OEMCP, $CP-MACCP, $CP-SYMBOL, $CP-UTF7,
	$CP-UTF8;
  export $CTRY-DEFAULT, $CTRY-ALBANIA, $CTRY-ALGERIA, $CTRY-ARGENTINA,
	$CTRY-AUSTRALIA, $CTRY-AUSTRIA, $CTRY-BAHRAIN, $CTRY-BELARUS,
	$CTRY-BELGIUM, $CTRY-BELIZE, $CTRY-BOLIVIA, $CTRY-BRAZIL,
	$CTRY-BRUNEI-DARUSSALAM, $CTRY-BULGARIA, $CTRY-CANADA,
	$CTRY-CARIBBEAN, $CTRY-CHILE, $CTRY-COLOMBIA, $CTRY-COSTA-RICA,
	$CTRY-CROATIA, $CTRY-CZECH, $CTRY-DENMARK, $CTRY-DOMINICAN-REPUBLIC,
	$CTRY-ECUADOR, $CTRY-EGYPT, $CTRY-EL-SALVADOR, $CTRY-ESTONIA,
	$CTRY-FAEROE-ISLANDS, $CTRY-FINLAND, $CTRY-FRANCE, $CTRY-GERMANY,
	$CTRY-GREECE, $CTRY-GUATEMALA, $CTRY-HONDURAS, $CTRY-HONG-KONG,
	$CTRY-HUNGARY, $CTRY-ICELAND, $CTRY-INDIA, $CTRY-INDONESIA,
	$CTRY-IRAN, $CTRY-IRAQ, $CTRY-IRELAND, $CTRY-ISRAEL, $CTRY-ITALY,
	$CTRY-JAMAICA, $CTRY-JAPAN, $CTRY-JORDAN, $CTRY-KENYA, $CTRY-KUWAIT,
	$CTRY-LATVIA, $CTRY-LEBANON, $CTRY-LIBYA, $CTRY-LIECHTENSTEIN,
	$CTRY-LITHUANIA, $CTRY-LUXEMBOURG, $CTRY-MACAU, $CTRY-MACEDONIA,
	$CTRY-MALAYSIA, $CTRY-MEXICO, $CTRY-MONACO, $CTRY-MOROCCO,
	$CTRY-NETHERLANDS, $CTRY-NEW-ZEALAND, $CTRY-NICARAGUA, $CTRY-NORWAY,
	$CTRY-OMAN, $CTRY-PAKISTAN, $CTRY-PANAMA, $CTRY-PARAGUAY, $CTRY-PERU,
	$CTRY-PHILIPPINES, $CTRY-POLAND, $CTRY-PORTUGAL, $CTRY-PRCHINA,
	$CTRY-PUERTO-RICO, $CTRY-QATAR, $CTRY-ROMANIA, $CTRY-RUSSIA,
	$CTRY-SAUDI-ARABIA, $CTRY-SERBIA, $CTRY-SINGAPORE, $CTRY-SLOVAK,
	$CTRY-SLOVENIA, $CTRY-SOUTH-AFRICA, $CTRY-SOUTH-KOREA, $CTRY-SPAIN,
	$CTRY-SWEDEN, $CTRY-SWITZERLAND, $CTRY-SYRIA, $CTRY-TAIWAN,
	$CTRY-THAILAND, $CTRY-TRINIDAD-Y-TOBAGO, $CTRY-TUNISIA, $CTRY-TURKEY,
	$CTRY-UAE, $CTRY-UKRAINE, $CTRY-UNITED-KINGDOM, $CTRY-UNITED-STATES,
	$CTRY-URUGUAY, $CTRY-VENEZUELA, $CTRY-VIET-NAM, $CTRY-YEMEN,
	$CTRY-ZIMBABWE;
  export $LOCALE-NOUSEROVERRIDE, $LOCALE-USE-CP-ACP,
	$LOCALE-RETURN-NUMBER, $LOCALE-ILANGUAGE, $LOCALE-SLANGUAGE,
	$LOCALE-SENGLANGUAGE, $LOCALE-SABBREVLANGNAME,
	$LOCALE-SNATIVELANGNAME, $LOCALE-ICOUNTRY, $LOCALE-SCOUNTRY,
	$LOCALE-SENGCOUNTRY, $LOCALE-SABBREVCTRYNAME,
	$LOCALE-SNATIVECTRYNAME, $LOCALE-IDEFAULTLANGUAGE,
	$LOCALE-IDEFAULTCOUNTRY, $LOCALE-IDEFAULTCODEPAGE,
	$LOCALE-IDEFAULTANSICODEPAGE, $LOCALE-IDEFAULTMACCODEPAGE,
	$LOCALE-SLIST, $LOCALE-IMEASURE, $LOCALE-SDECIMAL, $LOCALE-STHOUSAND,
	$LOCALE-SGROUPING, $LOCALE-IDIGITS, $LOCALE-ILZERO,
	$LOCALE-INEGNUMBER, $LOCALE-SNATIVEDIGITS, $LOCALE-SCURRENCY,
	$LOCALE-SINTLSYMBOL, $LOCALE-SMONDECIMALSEP, $LOCALE-SMONTHOUSANDSEP,
	$LOCALE-SMONGROUPING, $LOCALE-ICURRDIGITS, $LOCALE-IINTLCURRDIGITS,
	$LOCALE-ICURRENCY, $LOCALE-INEGCURR, $LOCALE-SDATE, $LOCALE-STIME,
	$LOCALE-SSHORTDATE, $LOCALE-SLONGDATE, $LOCALE-STIMEFORMAT,
	$LOCALE-IDATE, $LOCALE-ILDATE, $LOCALE-ITIME, $LOCALE-ITIMEMARKPOSN,
	$LOCALE-ICENTURY, $LOCALE-ITLZERO, $LOCALE-IDAYLZERO,
	$LOCALE-IMONLZERO, $LOCALE-S1159, $LOCALE-S2359,
	$LOCALE-ICALENDARTYPE, $LOCALE-IOPTIONALCALENDAR,
	$LOCALE-IFIRSTDAYOFWEEK, $LOCALE-IFIRSTWEEKOFYEAR, $LOCALE-SDAYNAME1,
	$LOCALE-SDAYNAME2, $LOCALE-SDAYNAME3, $LOCALE-SDAYNAME4,
	$LOCALE-SDAYNAME5, $LOCALE-SDAYNAME6, $LOCALE-SDAYNAME7,
	$LOCALE-SABBREVDAYNAME1, $LOCALE-SABBREVDAYNAME2,
	$LOCALE-SABBREVDAYNAME3, $LOCALE-SABBREVDAYNAME4,
	$LOCALE-SABBREVDAYNAME5, $LOCALE-SABBREVDAYNAME6,
	$LOCALE-SABBREVDAYNAME7, $LOCALE-SMONTHNAME1, $LOCALE-SMONTHNAME2,
	$LOCALE-SMONTHNAME3, $LOCALE-SMONTHNAME4, $LOCALE-SMONTHNAME5,
	$LOCALE-SMONTHNAME6, $LOCALE-SMONTHNAME7, $LOCALE-SMONTHNAME8,
	$LOCALE-SMONTHNAME9, $LOCALE-SMONTHNAME10, $LOCALE-SMONTHNAME11,
	$LOCALE-SMONTHNAME12, $LOCALE-SMONTHNAME13,
	$LOCALE-SABBREVMONTHNAME1, $LOCALE-SABBREVMONTHNAME2,
	$LOCALE-SABBREVMONTHNAME3, $LOCALE-SABBREVMONTHNAME4,
	$LOCALE-SABBREVMONTHNAME5, $LOCALE-SABBREVMONTHNAME6,
	$LOCALE-SABBREVMONTHNAME7, $LOCALE-SABBREVMONTHNAME8,
	$LOCALE-SABBREVMONTHNAME9, $LOCALE-SABBREVMONTHNAME10,
	$LOCALE-SABBREVMONTHNAME11, $LOCALE-SABBREVMONTHNAME12,
	$LOCALE-SABBREVMONTHNAME13, $LOCALE-SPOSITIVESIGN,
	$LOCALE-SNEGATIVESIGN, $LOCALE-IPOSSIGNPOSN, $LOCALE-INEGSIGNPOSN,
	$LOCALE-IPOSSYMPRECEDES, $LOCALE-IPOSSEPBYSPACE,
	$LOCALE-INEGSYMPRECEDES, $LOCALE-INEGSEPBYSPACE,
	$LOCALE-FONTSIGNATURE, $LOCALE-SISO639LANGNAME,
	$LOCALE-SISO3166CTRYNAME;
  export $TIME-NOMINUTESORSECONDS, $TIME-NOSECONDS,
	$TIME-NOTIMEMARKER, $TIME-FORCE24HOURFORMAT;
  export $DATE-SHORTDATE, $DATE-LONGDATE, $DATE-USE-ALT-CALENDAR;
  export $CAL-ICALINTVALUE, $CAL-SCALNAME, $CAL-IYEAROFFSETRANGE,
	$CAL-SERASTRING, $CAL-SSHORTDATE, $CAL-SLONGDATE, $CAL-SDAYNAME1,
	$CAL-SDAYNAME2, $CAL-SDAYNAME3, $CAL-SDAYNAME4, $CAL-SDAYNAME5,
	$CAL-SDAYNAME6, $CAL-SDAYNAME7, $CAL-SABBREVDAYNAME1,
	$CAL-SABBREVDAYNAME2, $CAL-SABBREVDAYNAME3, $CAL-SABBREVDAYNAME4,
	$CAL-SABBREVDAYNAME5, $CAL-SABBREVDAYNAME6, $CAL-SABBREVDAYNAME7,
	$CAL-SMONTHNAME1, $CAL-SMONTHNAME2, $CAL-SMONTHNAME3,
	$CAL-SMONTHNAME4, $CAL-SMONTHNAME5, $CAL-SMONTHNAME6,
	$CAL-SMONTHNAME7, $CAL-SMONTHNAME8, $CAL-SMONTHNAME9,
	$CAL-SMONTHNAME10, $CAL-SMONTHNAME11, $CAL-SMONTHNAME12,
	$CAL-SMONTHNAME13, $CAL-SABBREVMONTHNAME1, $CAL-SABBREVMONTHNAME2,
	$CAL-SABBREVMONTHNAME3, $CAL-SABBREVMONTHNAME4,
	$CAL-SABBREVMONTHNAME5, $CAL-SABBREVMONTHNAME6,
	$CAL-SABBREVMONTHNAME7, $CAL-SABBREVMONTHNAME8,
	$CAL-SABBREVMONTHNAME9, $CAL-SABBREVMONTHNAME10,
	$CAL-SABBREVMONTHNAME11, $CAL-SABBREVMONTHNAME12,
	$CAL-SABBREVMONTHNAME13;
  export $ENUM-ALL-CALENDARS;
  export $CAL-GREGORIAN, $CAL-GREGORIAN-US, $CAL-JAPAN, $CAL-TAIWAN,
	$CAL-KOREA, $CAL-HIJRI, $CAL-THAI, $CAL-HEBREW,
	$CAL-GREGORIAN-ME-FRENCH, $CAL-GREGORIAN-ARABIC,
	$CAL-GREGORIAN-XLIT-ENGLISH, $CAL-GREGORIAN-XLIT-FRENCH;
  export <CALTYPE>;
  export <CALID>;
  export MaxCharSize-value, MaxCharSize-value-setter,
	DefaultChar-array, DefaultChar-array-setter, DefaultChar-value,
	LeadByte-array, LeadByte-array-setter, LeadByte-value, <CPINFO>,
	<LPCPINFO>;
  export MaxCharSize-value, MaxCharSize-value-setter,
	DefaultChar-array, DefaultChar-array-setter, DefaultChar-value,
	LeadByte-array, LeadByte-array-setter, LeadByte-value,
	UnicodeDefaultChar-value, UnicodeDefaultChar-value-setter,
	CodePage-value, CodePage-value-setter, CodePageName-array,
	CodePageName-array-setter, CodePageName-value, <CPINFOEXA>,
	<LPCPINFOEXA>, <CPINFOEX>, <LPCPINFOEX>;
  export NumDigits-value, NumDigits-value-setter, LeadingZero-value,
	LeadingZero-value-setter, Grouping-value, Grouping-value-setter,
	lpDecimalSep-value, lpDecimalSep-value-setter, lpThousandSep-value,
	lpThousandSep-value-setter, NegativeOrder-value,
	NegativeOrder-value-setter, <NUMBERFMTA>, <LPNUMBERFMTA>,
	<NUMBERFMT>, <LPNUMBERFMT>;
  export NumDigits-value, NumDigits-value-setter, LeadingZero-value,
	LeadingZero-value-setter, Grouping-value, Grouping-value-setter,
	lpDecimalSep-value, lpDecimalSep-value-setter, lpThousandSep-value,
	lpThousandSep-value-setter, NegativeOrder-value,
	NegativeOrder-value-setter, PositiveOrder-value,
	PositiveOrder-value-setter, lpCurrencySymbol-value,
	lpCurrencySymbol-value-setter, <CURRENCYFMTA>, <LPCURRENCYFMTA>,
	<CURRENCYFMT>, <LPCURRENCYFMT>;
  export <LOCALE-ENUMPROCA>, <LOCALE-ENUMPROC>-callback-wrapper,
	<CODEPAGE-ENUMPROCA>, <CODEPAGE-ENUMPROC>-callback-wrapper,
	<DATEFMT-ENUMPROCA>, <DATEFMT-ENUMPROC>-callback-wrapper,
	<DATEFMT-ENUMPROCEXA>, <DATEFMT-ENUMPROCEX>-callback-wrapper,
	<TIMEFMT-ENUMPROCA>, <TIMEFMT-ENUMPROC>-callback-wrapper,
	<CALINFO-ENUMPROCA>, <CALINFO-ENUMPROC>-callback-wrapper,
	<CALINFO-ENUMPROCEXA>, <CALINFO-ENUMPROCEX>-callback-wrapper;
  export <LOCALE-ENUMPROC>, <CODEPAGE-ENUMPROC>, <DATEFMT-ENUMPROC>,
	<DATEFMT-ENUMPROCEX>, <TIMEFMT-ENUMPROC>, <CALINFO-ENUMPROC>,
	<CALINFO-ENUMPROCEX>;
  export IsValidCodePage, GetACP, GetOEMCP, GetCPInfo, GetCPInfoEx,
	IsDBCSLeadByte, IsDBCSLeadByteEx, MultiByteToWideChar,
	WideCharToMultiByte;
  export CompareString, LCMapString, GetLocaleInfo, SetLocaleInfo,
	GetTimeFormat, GetDateFormat, GetNumberFormat, GetCurrencyFormat,
	EnumCalendarInfo, EnumTimeFormats, EnumDateFormats, IsValidLocale,
	ConvertDefaultLocale, GetSystemDefaultLangID, GetUserDefaultLangID,
	GetSystemDefaultLCID, GetUserDefaultLCID;
  export GetStringTypeEx;
  export FoldString, EnumSystemLocales, EnumSystemCodePages;

  // special case in "kernhack.dylan":
  export GetLargestConsoleWindowSize;
  export FormatMessage;
  export <LCTYPE>;
  export <LPLPSTR>, <LPLPCSTR>, <LPLPOVERLAPPED>;
  export <LPLARGE-INTEGER>, <DWORDLONG>, <PVOID64>;

  // utility functions:
  export win32-error-message,
         check-win32-result,
         report-win32-error,
         ensure-no-win32-error;

  // shared slot accessors:
  export dwPlatformId-value, cBytes-value, dwPlatformId-value, cBytes-value;
  export dwPlatformId-value-setter, cBytes-value-setter,
    dwPlatformId-value-setter, cBytes-value-setter;

  // accessors for "WinMain" parameters:
  export application-instance-handle, application-command-line,
    application-show-window;

end module Win32-kernel;
