
! Additional special case rules for translating "winsock2.h".
! This version works with the header files from the 
! MSDN "Platform SDK" of October 1997 or January 1998.
!
! $HopeName: D-win32-winsock2!more.pat(D-kan.4) $
! $Date: 2004/03/12 00:09:30 $

@set-switch{t;1}@set-switch{w;1}


\/\*\ Winsock\J*\n\G\s\*\s<header>\*\/=$0 

\#define\ WINSOCK_API_LINKAGE DECLSPEC_IMPORT=
WINSOCK_API_LINKAGE=

struct <I>\W\{<fields>\}\W\;=\N\n\
	@bind{sclass;@map-name{$1}}\
	@define{type\:struct\s\W$1\<stars\>\=\@resolve-type\{\$1$1\}\@end}\
	@define{ptr-type\:\\J@quote{$1}\=@quote{LP@upcase{@map-name{$1}}}}\
	define C-struct @export{\<${sclass}\>}\n$2\N\
	\ \ pointer-type-name\: @export{\<LP@upcase{@map-name{$1}\>}}\;\n\
	end C-struct\;\N@unbind{sclass}

! ignore backward compatibility hack:
fields:\#define h_addr *\n=

fields:unsigned\ char <I>\[\W<number>\]<space>\;=\
	\ \ array slot @export{$1-array} @tab{20}\:\:\
	@wrap{\ @type{unsigned char},}@wrap{\ length\: $2,}\
	@ignore{@export{$1-array-setter}}\
	@wrap{\ address-getter\: @export{$1-value}\;}\n

! special case because the nested unions are more trouble than they are worth.
struct\ in_addr\W\{<matchparen>\}\;=\
	define constant @export{\<in-addr\>} \= \<C-raw-unsigned-long\>\;\n
type:struct\s\Win_addr<stars>=\<in-addr$1\>@end

! network address should always be treated as a <machine-word>, not <integer>.
type:u_long \PWSAAPI=\<C-raw-unsigned-long\>@end
type:unsigned\ long \PWSAAPI=\<C-raw-unsigned-long\>@end
args:u_long hostlong=\ \ @putparm{hostlong,\<C-raw-unsigned-long\>}
args:u_long netlong=\ \ @putparm{netlong,\<C-raw-unsigned-long\>}

link:PASCAL\ FAR=, c-modifiers\: "__stdcall"@end
link:WSAAPI=, c-modifiers\: "__stdcall"@end
relevant-name:WSAAPI=@fail

! special case to use %logior instead of logior
number:(IOC_IN\|IOC_OUT)=\%logior(\$IOC-IN, \$IOC-OUT)@end

number:(SOCKET)=
number:(u_int)=
number:(INFINITE)=\$FFFFFFFF@end
number:(WSAEVENT)NULL=\$NULL-HANDLE@end
number:<I>(<expr-list>)=@map-name{$1}($2)@end

type:struct\ fd_set<stars>=\<fd-set$1\>@end
type:u_long\ FAR\W\*=\<C-both-unsigned-long\*\>@end
type:u_short\ FAR\W\*=\<C-unsigned-short\*\>@end
type:struct\ sockaddr\ FAR\W\*=\<LPSOCKADDR\>@end
type:struct\ sockaddr\W\*\*=\<sockaddr\*\*\>@end

! handle SOCKET as a special case - always use a <machine-word>
typedef\ u_int SOCKET\;=
type:SOCKET<stars>=\<C-SOCKET$1\>@end

! leading underscore special because Dylan doesn't allow beginning with "-".
map-name:\A_\J=\%
export:\A\_\J*=@map-name{$0}

! gz, Mar-10-00: No longer imported from win32-common
!
! defined in win32-common; don't re-export:
!export-slot:cbSize=cbSize-value
!export-slot:dwSize=dwSize-value
!export-slot:Flags=Flags-value
!export-slot:u=u-value
!export-slot:wVersion=wVersion-value
! special case because no setter:
export:Buffer-value=$0

! C function, Dylan reserved word
map-name:select=winsock-select

args:IN OUT <type>\W<i>=\
 \ \ input output parameter @cmps{$2;;$2;@argname{$1};$2} @tab{24}\:\: $1\;\n
args:IN <type>\W<i>=\
 \ \ input parameter @cmps{$2;;$2;@argname{$1};$2} @tab{24}\:\: $1\;\n
args:OUT <type>\W<i>=\
 \ \ output parameter @cmps{$2;;$2;@argname{$1};$2} @tab{24}\:\: $1\;\n

typedef\ struct\ <I> \C$1\;=
typedef\ struct\ <I> <stars><lp>\J\C$1\;=
lp:P\J=@end;LP\J=@end;=@end

! Function-like macros

\L\#define <relevant-function>\J(<matchparen>) <defexpr>\n=\
	\N\ndefine inline-only function @export{$1} ($2)\;\n\ \ $3\nend\;\n
defexpr:\\\n\W=\S
defexpr:<I>(<expr-list>)=@map-name{$1}($2)
defexpr:<S>=$1
defexpr:(SOCKET)=
defexpr:(fd_set FAR\*)(<defexpr>)=pointer-cast(\<LPFD-SET\>, $1)
defexpr:\-\J\><I>=.@map-name{$1-value}
defexpr:((long)(<defexpr>) \& <number>)=\%logand($1, $2)
defexpr:(<J>)\W\<\<\W<number>=ash($1,$2)
defexpr:(IOC_\J<K><or-args>)=\%logior(\$IOC-$1$2)
number:_IO\J<K>('<char>',\W<number>,\W<type>)=\
	@map-name{_IO$1}(as(\<integer\>, '$2'), $3, $4)@end
defexpr:((long)sizeof(<J>)\&<number>)\<\<<number>=\
	ash(logand(size-of($1), $2), $3)
defexpr:\=\=\W=\S\=\S
defexpr:\=\W=\S\:\=\S
defexpr:<J>\J<i>=\I$1$2
defexpr:<number>=$1
defexpr:(<J>)=\I$1
defexpr:(#)=(#)
defexpr:\/\*<comment>\*\/=
! expressions not yet handled:
defexpr:\{=@fail
defexpr:\?=@fail
defexpr:\/=@fail
defexpr:\*=@fail
defexpr:\|=@fail
defexpr:\&=@fail
expr-list:\S=\S
expr-list:<defexpr>=$1
expr-list:,\W<defexpr>=,@wrap{\s$1}
expr-list:<G>=@err{line @line failed expr-list at\: $1\n}@fail

or-args:\W\|(<defexpr>)=, $1
or-args:=@terminate

\N\#if\ INCL_WINSOCK_API_TYPEDEFS<endline><matchcond><elsepart>=@{$3}

! symbol macro can't be translated to Dylan:
relevant-constant:h_errno\I=@fail

! unused and undocumented:
relevant-name:_IO\I=@fail

! undefined and undocumented:
relevant-function:WSAGetAddressByName\J<opta>\I=@fail

! obsolete:
relevant-function:WSACancelBlockingCall=@fail
relevant-function:WSAIsBlocking=@fail
relevant-function:WSAUnhookBlockingHook=@fail
relevant-function:WSASetBlockingHook=@fail

! complex macros that need to be translated by hand:
relevant-function:timercmp=@fail
relevant-function:timerisset=@fail
relevant-function:FD_CLR=@fail
relevant-function:FD_SET=@fail
relevant-function:FD_ZERO=@fail

! already defined in win32-common:
def-flag:MAKEWORD=$0@end

! Mistake in header file -- IOC_W32 is undefined.
! SIO_QUERY_TARGET_PNP_HANDLE is undocumented anyway.
\#define\ SIO_QUERY_TARGET_PNP_HANDLE _WSAIOR(IOC_W32,24)=

! type aliases
\L\#define <K> <K><endline>=define constant @export{\<$1\>} = \<$2\>\;\n

! Warn about macros that couldn't be handled:
\L\#define <relevant-function>\J(*\n=@err{Not translated\: $0}

! special case arguments that look like output parameters but are not:
outok:lpFlags=@fail! WSARecv, WSARecvFrom - input/output parm.
outok:lpdwBufferLength=@fail! WSAEnumProtocols - input/output parm.
