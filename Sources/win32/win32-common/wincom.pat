
! This file extends "patterns.pat" with rules specific to
! the win32-common library.

! Copyright: 1996-2000 Functional Objects, Inc.  All rights reserved.

number:<K>\W(<number>,\W<number>)=$0@end

! exclude error codes that belong to the COM or OLE modules:
\/\/\s\s\L OLE Error Codes \/\/=\
  @err{\ Excluding OLE Error Codes and all following.\n}\N@export-end{}@end

! other unneeded groups of error codes:
\N\/\/\J\/\/<U>\n\G\/\/ \/\/\n\/\/ <exclude-errs>\W\/\/\G\n\/\/ \/\/\n\/\/\/\/*\n<U>\P\/\J\/\J\/\J\/=@err{\ Excluding $2\n}@export-end{}
exclude-errs:OpenGL Error Code=$0@end
exclude-errs:Image Color Management Error Code=$0@end
exclude-errs:Win32 Spooler Error Codes=$0@end
exclude-errs:WMI Error Codes=$0@end
exclude-errs:RPC Status Codes=$0@end
exclude-errs:Security Status Codes=$0@end! NT only
exclude-errs:EFS Error Codes=$0@end! Encrypted File System new in NT 5.0
exclude-errs:Cluster Error Codes=$0@end! NT only
exclude-errs:Eventlog Status Codes=$0@end! NT only
exclude-errs:NT <P>=$0@end! still more NT only stuff

exclude-errs:\s=
exclude-errs:=@fail

! retain comment describing error codes:
\N\/\/\ MessageId\: <I>\W\n\/\/\n\/\/ MessageText\:\n\
	<message-text>\G\P\#define $1=\n$2
\N\/\/\n\P\/\/\ MessageId\:=! defeat rule in patterns.pat for long comments
message-text:\/\/\W\n=
message-text:\/\/\W*\n=\/\/ *\n
message-text:=@terminate


! special cases:
typedef\ BYTE\ BOOLEAN\;=! handled specially in "moretype.dylan"
typedef int BOOL\;=\Ndefine constant @export{\<BOOL\>} \= \<C-Boolean\>\;\n
typedef char <k>\JCHAR\;=\
	\Ndefine constant @export{\<$1CHAR\>} \= \<C-character\>\;\n
typedef wchar_t WCHAR\;=! handled specially in "moretype.dylan"
! gz, mar-04-00: this is now in win32-core:c-ffi.
!typedef void\*HANDLE\;=\
!	\Ndefine open C-subtype @export{\<HANDLE\>} ( \<C-void\*\> ) \
!	end\;\n

! these types are defined specially in "first.dylan" so ignore them here:
typedef\ LONG\ LPARAM\;=
typedef\ LONG\ LRESULT\;=
typedef\ UINT\ WPARAM\;=
typedef\ unsigned\ int UINT\;=
typedef\ unsigned\ int \*PUINT\;=

! gz, mar-04-00: these are now in win32-core:c-ffi.
!typedef\ void\ NEAR\* HGDIOBJ\;=\N\
!	define C-subtype @export{\<HGDIOBJ\>} ( \<HANDLE\> ) end\;\n
!
!DECLARE_HANDLE(<gdiobj>)\;=\N\
!	define C-subtype @export{\<$1\>} ( \<HGDIOBJ\> ) end\;\n
!DECLARE_HANDLE(<L>)\;=\Ndefine C-subtype @export{\<$1\>} ( \<HANDLE\> ) end\;\n
!
!gdiobj:HBITMAP=$0@end;HBRUSH=$0@end;HFONT=$0@end;HPALETTE=$0@end
!gdiobj:HPEN=$0@end;HRGN=$0@end! Are there other subclasses of HGDIOBJ???
!gdiobj:=@fail

! exclude error codes that belong to the COM or OLE modules.
relevant-constant:FACILITY_\J<L>=@fail
relevant-constant:RPC_\J<L1>\J_\J<L>=@fail

! this set of constants in "winnt.h" are unused, undocumented, and incorrect:
relevant-constant:\IMIN\J<K> 0x8\J=@fail

! winnt.h
relevant-constant:SIZEOF_RFPO_DATA=@fail! unused and undocumented

relevant-constant:HFILE\J<-L0>=@fail! used only by obsolete functions
relevant-name:PBOOLEAN=@fail! defined in "winnt.h" but never used

relevant-name:CCHAR=@fail! defined in "winnt.h" but never used


! don't want these
bad-struct:_CONTEXT=$0@end
bad-struct:_IMAGE_\J<I>=$0@end
bad-struct:_RTL_\J<I>=$0@end
bad-struct:_GUID=$0@end! defined in winnt.h but not used
bad-struct:_OBJECTID=$0@end! defined in winnt.h but not used
bad-struct:_FPO_DATA=$0@end! defined in winnt.h but not used


! used in "winnt.h" for some junk that doesn't translate well, so ignore:
\N\#if defined(MIDL_PASS)*\n<matchcond><elsepart>=
\N\#if \!defined(RC_INVOKED)*\n<matchcond><elsepart>=

! Don't yet include security features that only work on NT:
\N\#ifndef NOSECURITY<endline><matchcond><elsepart>=@{$3}


! ---- misc. ----

! this file needs to be embedded in "windef.dylan" in order for the data
! types to be defined in the right order.
\#include \<winnt.h\>=\N\n@{@read{@mergepath{@inpath;winnt.h;}}}\N\n\
	\/\/ --- end of data from \"winnt.h\" ---\n\n

! Don't yet need the low-level system stuff in "winnt.h"	???
! (virtual memory, security, devices, image files, etc.)
\N\#ifndef\ WIN32_NO_STATUS=\N@close-export{}@end

! Omit this from winnt.h:
\N\#define _DWORDLONG_\n\#if\ (*\n<matchcond>\#endif=

! Not needed here; defined in COM library:
def-flag:_HRESULT_DEFINED=@end

\#define\ EXTERN_C extern*\n=
