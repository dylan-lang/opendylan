
! rules shared by the Dylan and C generators for interfaces

! Copyright: 1998 Functional Objects, Inc.  All rights reserved.

! omit some undocumented interfaces
good-interface:IRpcChannelBuffer\J<D>=@fail
good-interface:IChannelHook=@fail
good-interface:ISynchronize=@fail
good-interface:ISynchronizeMutex=@fail
good-interface:IAsyncSetup=@fail
good-interface:IAsyncManager=@fail
good-interface:IWaitMultiple=@fail
good-interface:ISynchronizeEvent=@fail
good-interface:IUrlMon=@fail

! omit interfaces that are just for RPC internals
good-interface:IRpcOptions=@fail
good-interface:IComBinding=@fail
good-interface:ISurrogate=@fail

! omit other interfaces that users don't implement or use directly:
good-interface:IFillLockBytes=@fail
good-interface:IProgressNotify=@fail

! omit interfaces not supported on Windows 95:
good-interface:IGlobalInterfaceTable=@fail
\#if\W(_WIN32_WINNT\W\>\=\W0x0400\W)\W\|\|\Wdefined(_WIN32_DCOM)\
	<matchcond>\#endif\I=

! omit OLE Automation interfaces that are only used for "ActiveX designer":
good-interface:ITypeChangeEvents=@fail

good-interface:<use-only-interface>=@fail
good-interface:<I>=$1@end
good-interface:=@fail

! The following interfaces might be used by the user, but the system
! provides the only implementation, so we don't need to define a
! Dylan implementation class.
use-only-interface:ILayoutStorage=$0@end
use-only-interface:IClientSecurity=$0@end
use-only-interface:IServerSecurity=$0@end
use-only-interface:IRunningObjectTable=$0@end
use-only-interface:IEnumSTATDATA=$0@end
use-only-interface:IRpcChannelBuffer=$0@end
use-only-interface:IOleCacheControl=$0@end
use-only-interface:IOleAdviseHolder=$0@end
use-only-interface:IOleLink=$0@end
use-only-interface:IDirectWriterLock=$0@end
use-only-interface:ICancelMethodCalls=$0@end
use-only-interface:IMultiQI=$0@end

! Don't bother supporting implementation of a Class Store Provider:
use-only-interface:IClassAdmin=@fail
use-only-interface:IClassRefresh=$0@end
use-only-interface:IEnumPackage=$0@end
use-only-interface:IEnumClass=$0@end
use-only-interface:IClassAccess=$0@end

use-only-interface:=@fail

\#ifdef COBJMACROS\n<matchcond>\#endif=

! basic C syntax shared by "dinterf.pat" and "cinterf.pat":

\/\J\/*\n=
\/\J\*<comment>\*\J\/=
comment:\/\J\*<comment>\*\J\/=

! optional space
space:\/\J\/*\n= $0
space:\/\J\*<comment>\*\J\/= $0 ;
space:<S>=$1
space:\N\W\#*\n=$0
space:=@end

\"<string>\"=
string:\\?=\\?
\'<char>\'=
char:\\?=\\?@end;?=?@end

matchparen:(#)=(#)
matchparen:\{#\}=\{#\}
matchparen:\/\J\*<comment>\*\J\/=$0
matchparen:\/\J\/*\n=$0

matchcond:\N\#if\J*\n<matchcond>\#endif\I=$0
matchcond:\/\J\*<comment>\*\J\/=
matchcond:\/\J\/*\n=

