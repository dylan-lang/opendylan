
! This file extends "patterns.pat" with rules specific to
! the Win32 libraries.

! Copyright: 1998 Functional Objects, Inc.  All rights reserved.

! $HopeName$
! $Date: 2004/03/12 00:10:23 $


number:TRUE=1@end!  special case for FVIRTKEY in winuser.h

relevant-name:BCHAR=@fail! defined in "wingdi.h" but never used, not documented
relevant-name:\C<i>\JDEBUG\J=@fail
relevant-name:\C<i>\JTHREAD\J=@fail


! special case to avoid invalid output parameters (in "winbase.h"):
WINBASEAPI BOOL WINAPI SetNamedPipeHandleState(HANDLE hNamedPipe, LPDWORD lpMode, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout)\;=\N\n\
    define C-function @export{SetNamedPipeHandleState}\n\
    \ \ parameter hNamedPipe\t\:\: <HANDLE>\;\n\
    \ \ parameter lpMode\t\:\: <LPDWORD>\;\n\
    \ \ parameter lpMaxCollectionCount\t\:\: <LPDWORD>\;\n\
    \ \ parameter lpCollectDataTimeout\t\:\: <LPDWORD>\;\n\
    \ \ result value \:\: <BOOL>\;\n\
    \ \ c-name\: "SetNamedPipeHandleState", c-modifiers\: "__stdcall"\;\n\
    end\;\n

! the only functions beginning with "_" are obsolete.
relevant-function:\I_\J<I>=@fail
relevant-function:<i>\JThread\J=@fail
relevant-function:OpenFile=@fail! obsolete
relevant-function:GetStringType\J/[AW]/\I=@fail! obsolete, inconsistent
! omit as part of thread support and because of inappropriate output parm.:
relevant-function:Interlocked\J<K1><J>\I=@fail
! needs <PMEMORY-BASIC-INFORMATION> from omitted part of winnt.h:   ???
relevant-function:VirtualQuery\J=@fail
! only use of omitted low-level structure LPDEBUG_EVENT:
relevant-function:WaitForDebugEvent=@fail
! handled specially because it tries to return a structure by value:
relevant-function:GetLargestConsoleWindowSize=@fail
! only use of problematic structure (unnamed union), and low-level NT-only ???
relevant-function:HeapWalk=@fail

! special case arguments that look like output parameters but are not:
outok:ProcessHeaps=@fail! GetProcessHeaps - array pointer
outok:pHandles=@fail! MsgWaitForMultipleObjects - array pointer
outok:lpDistanceToMoveHigh=@fail! SetFilePointer - input/output parm.
outok:lpi=@fail! IsTextUnicode  - input/output parm.
outok:lpContext=@fail! BackupRead,BackupSeek,BackupWrite - input/output
outok:lpdw\J<L>\JSize=@fail ! MakeAbsoluteSD  - input/output
! the following group of parameters are used in "winbase" functions
! "Lookup..." for both input and output:
outok:cb\J<K1>\J<I>=@fail
outok:nSize=@fail! GetComputerName, GetUserName - input/output
outok:paFormatPriorityList=@fail! GetPriorityClipboardFormat - array
outok:lpChar=@fail! ToAscii - array
outok:lpnTabStopPositions=@fail! TabbedTextOut, GetTabbedTextExtent - array
outok:lpAttribute=@fail! ReadConsoleOutputAttribute - array
outok:lpCharType=@fail! "winnls.h" GetStringTypeEx - array
outok:lpu\J<L>\JDirLen=@fail! "winver.h" VerFindFile - i/o
outok:lpuTmpFileLen=@fail! VerInstallFile - i/o


typedef <type>(\WCALLBACK\W\*\W<relevant-name>)(\W<typelist>)\;=\
  \Ndefine C-subtype @export{\<$2\>} ( \<C-function-pointer\> ) end\;\n

! Don't want Unicode versions of data types yet:
relevant-name:<K>\JW\I=@fail
bad-struct:tag\J<I>\JW\I=$0@end
bad-struct:_\J<I>\JW\I=$0@end
