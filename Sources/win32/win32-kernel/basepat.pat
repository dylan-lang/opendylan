! Synopsis:  Additional translation rules for "winbase.h".
! Author:    David N. Gray
! Copyright: 1996, 1998 Functional Objects, Inc.  All rights reserved.

! $HopeName$
! $Date: 2004/03/12 00:10:26 $

! This version has been tuned to work with the SDK files in the
!  October 1997 MSDN distribution.

! might not fit in a <small-integer>, so use <C-both-long>:
args:DWORD dw\J<big-arg>=\ \ parameter dw$1 \:\: \<C-both-unsigned-long\>\;\n
big-arg:\JDesiredAccess=$0@end
big-arg:\JFlagsAndAttributes=$0@end
big-arg:\JOpenMode=$0@end
big-arg:=@fail

type:DWORD\ \PdwLow\J=\<C-both-unsigned-long\>@end
type:DWORD\ \PdwHigh\J=\<C-both-unsigned-long\>@end

! for MulDiv:
type:int\ n\J=\<C-both-int\>@end

type:PVOID\W\*=\<C-void\*\*\>@end

number:(ATOM)=

! special case for enum defined in winnt.h and used in winbase.h:
type:LATENCY_TIME\I<stars>=\<C-int$1\>@end

! special hack for SYSTEM_INFO in winbase.h:
fields:union\{\W<I> <I>\;\W\/\/ Obsolete*\n\Wstruct\{<fields>\}\;\W\}\;=$4
matchparen2:union\{DWORD dwOemId\; \/\/ Obsolete*\n\Wstruct\{#\}\;\}\;=$2

! conditionalization for processor kind:
false-flag:(defined(_M_MRX000)||defined(_M_ALPHA)||(defined(_M_PPC)&&(_MSC_VER\>\=1000)))&&\!defined(RC_INVOKED)=$0@end
def-flag:_X86_=$0@end
undef-flag:_M_ALPHA=$0@end

! Don't want platform dependent stuff:
false-flag:(_WIN32_WINNT\>\=0x0400)\|\|(_WIN32_WINDOWS\>0x0400)=$0

! Don't include stuff that is only used for implementing debuggers:
\/\/\n\/\/ Debug APIs\n\/\/\n<U>typedef\ PEXCEPTION_POINTERS LPEXCEPTION_POINTERS\;=

! But do need these:
relevant-function:\IDebugBreak\J<opta>\I=$0@end
relevant-function:\IOutputDebugString\J<opta>\I=$0@end


! Don't include thread-related stuff:
relevant-function:<i>\JCriticalSection\J=@fail
def-flag:_NTOS_\I=$0@end
relevant-name:<i>\JCRITICAL_SECTION\J=@fail
relevant-name:LPLDT_ENTRY=@fail
relevant-function:UnhandledExceptionFilter=@fail


! Only used by QueryRecoveryAgents which isn't defined in NT 4.0:
bad-struct:_RECOVERY_AGENT_INFORMATION\J<opta>\I=$0@end
