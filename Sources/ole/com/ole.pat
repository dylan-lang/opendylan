
! Additional translation rules specific to COM, OLE, and OLESTD

! Copyright: 1996, 1998 Functional Objects, Inc.  All rights reserved.

! special handling for "objbase.h":


! These two rules will be created automatically when processing "objbase.h",
! but need to be defined here for "ole2.h".
:WINOLEAPI <relevant-function>\G(<args>)\;=@stdfun{\<C-HRESULT\>\d$1\d$2}
:WINOLEAPI_(<type>)\W<relevant-function>\G(<args>)\;=@stdfun{$1\d$2\d$3}

! skip arglist of omitted functions to avoid warnings:
WINOLEAPI <omitted-function>(<matchparen>)\;=
__RPC_\J<K> <omitted-function>(<matchparen>)\;=
STD\J<K> <omitted-function>(<matchparen>)\;=
__RPC_FAR\W\*\W__RPC_USER <omitted-function>(<matchparen>)\;=

omitted-function:<relevant-function>=@fail
omitted-function:<I>=$1@end
omitted-function:=@fail

! stdfun{type name args}
stdfun:*\d*\d*=\
	\N\ndefine C-function @export{@fnname{$2}}\n$3\N\
	@result{$1}\
	\ \ c-name\: \"$2\",@wrap{\ c-modifiers\: \"__stdcall\"\;}\n\
	end\;\n

:interface <I><t60>\{<matchparen>\}\;=

! special case to handle (in objidl.h):    void __RPC_FAR *reserved2[ 5 ];
fields:void\ __RPC_FAR\W\*\W<I>\W\[\W<number>\W\]<space>\;=\
	\ \ array slot @export{$1-array} @tab{20}\:\:\
	@wrap{\ \<C-void\*\>,}@wrap{\ length\: $2,}\
	@ignore{@export{$1-array-setter}}\
	@wrap{\ address-getter\: @export{$1-value}\;}\n

def-flag:NONAMELESSUNION=$0@end

type:HRESULT<stars>=\<C-HRESULT$1\>@end
type:SCODE<stars>=\<C-HRESULT$1\>@end! used in "samples/include/olestd.h"
! Special-case to support smart LPSTREAM
type:\IIStream<optstar>=\<LPSTREAM$1\>@end
type:I\J<K1>\J<J1>\J<L><optstar>=\<Interface$4\> \/\* I$1$2$3 \*\/@end
type:OLECHAR __RPC_FAR\*<stars>=\<C-unicode-string$1\>@end
type:OLECHAR FAR\*<stars>=\<C-unicode-string$1\>@end
type:OLECHAR const FAR\*<stars>=\<C-unicode-string$1\>@end
type:byte __RPC_FAR\W\*<stars>=\<LPBYTE$1\>@end

optstar:\*<stars>=$1@end
optstar:__RPC_FAR=
optstar:FAR=
optstar:\S=
optstar:=@end

! avoid a warning from "patterns.pat":
resolve-type:\*\*Rem\J<K>=@map-name{Rem$1}\*\*
resolve-type:\*user\J<K>=@map-name{user$1}\*

ptr-type:LPUNKNOWN=Interface\*
ptr-type:LPSTREAM=Interface\*
ptr-type:LPDATAOBJECT=Interface\*
ptr-type:LPENUMFORMATETC=Interface\*
ptr-type:LPLOCKBYTES=Interface\*
ptr-type:\*LOGPALETTE=LPLOGPALETTE\*
ptr-type:ULARGE_INTEGER=PULARGE-INTEGER

! special case to support smarter lpstreams
typedef \/\*<p>\*\/\ IStream __RPC_FAR\W\*LPSTREAM\;=

typedef \/\*<p>\*\/\WI\J<K1>\J<J1>\J<I> __RPC_FAR \*LP\J<K><d>\;=\
	define constant @export{\<LP$5$6\>} \= \<Interface\>\;\n\
	@define{itype\:$5$6\\I=\$0\@end}

! this one doesn't follow the usual pattern above:
typedef\ IAdviseSink __RPC_FAR\W\*\WLPADVISESINK\;=\
	define constant @export{\<LPADVISESINK\>} \= \<Interface\>\;\n\
	@define{itype\:ADVISESINK\\I=\$0\@end}

! special case to avoid generating a duplicate definition:
typedef\ struct\ tagOleMenuGroupWidths __RPC_FAR\W\*LPOLEMENUGROUPWIDTHS\;=

result:\<C-HRESULT\>=\ \ result status \:\: \<C-HRESULT\>\;\n

stars:\&=\*
stars:\*\ const=\*

typedef\ GUID <relevant-name>\;=\
	\Ndefine inline constant @export{@type{$1}} \= \<GUID\>\;\n\
  @define{typedef $1 __RPC_FAR\\\*\WLP$1\\\;\=\\N\
     define inline constant \@export\{@type{LP$1}\} \\\= \<LPGUID\>\\\;\\n}

\L\#define <K> EXTERN_C <declspec>\W<type>\W<link><endline>=\
  @define{$1 \<relevant-function\>\\G(\<args\>)\\\;\=\
	\\N\\ndefine C-function \@export\{\@fnname\{\$1\}\}\\n\$2\\N\
	\@result\{@quote{$3}\}\
	\\ \\ c-name\\\: \\"\$1\\"\@wrap\{@quote{$4\;}\}\\n\
	end\\\;\\n}

\L\#define <I>(<I>) EXTERN_C <declspec>\W$2 <link><endline>=\
  @define{$1(<type>)\\W\<relevant-function\>\\G(\<args\>)\\\;\=\
	\\N\\ndefine C-function \@export\{\@fnname\{\$2\}\}\\n\$3\\N\
	\@result\{\$1\}\
	\\ \\ c-name\\\: \\"\$2\\"\@wrap\{@quote{$4\;}\}\\n\
	end\\\;\\n}

! additional rules for output parameters in non-member functions:
args:LP\J<itype> FAR\*\W<I>=\
    \ \ output parameter $2 @tab{24}\:\: \<C-interface\*\>\;\
	\ \/\/ \<LP$1\>\n
itype:\JSTREAM\I=$0@end! for CreateStreamOnHGlobal in "omisc.dylan"
itype:\JDATAADVISEHOLDER\I=$0@end! for CreateDataAdviseHolder in "omisc.dylan"
itype:=@fail
args:LP\J<K> FAR\*\W<outok>=@outparm{LP$1 FAR\* $2}
args:LPUNKNOWN \*ppunkMarshal=@outparm{LPUNKNOWN\* ppunkMarshal}
args:LPSTREAM \*ppStm=@outparm{LPSTREAM\* ppStm}
args:BOOL FAR\W\*\W<outok>=@outparm{LPBOOL $1}
! for StgCreateDocfile etc. in "objbase.h":
args:I\J<K1>\J<J><i> FAR\W\*\WFAR\W\*<outok>=@outparm{I$1$2$3\*\* $4}

! unnamed output parameter for ParseCmdLine in "olestd.h":
outok:\A\P,=out${argnum}@incr{argnum}@end

! additional output parameters:
args:DWORD FAR\W\*\W<outok>=@outparm{DWORD\* $1}
args:WORD FAR\W\*\W<outok>=@outparm{WORD\* $1}
args:ULONG FAR\W\*\W<outok>=@outparm{ULONG\* $1}
args:H\J<K> FAR\W\*\W<outok>=@outparm{H$1\* $2}

! special case for functions in "ole2.h" that return an interface as a void*
args:LPVOID\ FAR\*\W<return-interface>\I=\
    \ \ output parameter $1 @tab{24}\:\: \<C-interface\*\>\;\n
return-interface:ppvObj=$0@end;lplpObj=$0@end;=@fail

declspec:DECLSPEC_IMPORT=__declspec(dllimport)@end
declspec:=@end

! Special hack for a case in "oleidl.h" that doesn't follow usual pattern:
type:struct\ tagOIFI<stars>=@resolve-type{$1OLEINPLACEFRAMEINFO}@end

link:STDMETHODCALLTYPE=, c-modifiers\: "__stdcall"@end
link:__RPC_USER=, c-modifiers\: "__stdcall"@end

type:TYMED\I=\<C-int\>@end! enumeration type defined in "objidl.h"
:STDAPI <relevant-function>\G(<args>)\;=@stdfun{\<C-HRESULT\>\d$1\d$2}
:STDAPI_(<type>)\W<relevant-function>\G(<args>)\;=@stdfun{$1\d$2\d$3}
:STDMETHODIMP <relevant-function>\G(<args>)\;=@stdfun{\<C-HRESULT\>\d$1\d$2}
undef-flag:OBSOLETE=$0@end
undef-flag:\s=

! for "samples/include/ansiapi.h":
\L\#define <I>\n=@define{def-flag\:$1\=\$0\@end}
def-flag:=@fail

! have to undefine this for compatibility with Windows 95.
undef-flag:_WIN32_DCOM=$0@end

false-flag:DBG\=\=1=$0

! for "samples/include/ole2ui.h":
type:OLECHAR=\<WCHAR\>@end

! for VARENUM in "oaidl.h" [note that this is C++ syntax, not standard C]
\Nenum <relevant-name>\W\{\W<enumbody>\}\G\W\;=\N\/\/ enum $1\:\n$2\n\
	@set{prev;-1}@export-end{}

! need <machine-word> addition for these:
enumbody:OLE_E_\J<I>\W\=\W<I>\+\W<number><term>=\
	define inline-only constant @export{\$OLE_E_$1} \=\
	@wrap{\ \\\%\+(\$@map-name{$2}, $3)\;}\n@set{prev;\$OLE-E-$1}
enumbody:OLE_E_\J<I><space><term>=\
	define inline-only constant @export{\$OLE_E_$1} \=\
	@wrap{\ \\\%\+(${prev;-1}, 1)\;}\n@set{prev;\$@map-name{OLE_E_$1}}

! for "oleauto.h":
args:double FAR\* <outok>=@outparm{$0}
args:unsigned short FAR\* <outok>=@outparm{$0}

! looks like a function name but is really a type name:
relevant-name:StorageLayout=$0@end

! exclude this structure used only in stub functions:
bad-struct:_FLAG_STGMEDIUM=$0@end

! structure PROPVARIANT doesn't follow the usual pattern
typedef\ struct\ tagPROPVARIANT PROPVARIANT\;=
struct tag\J<relevant-name>\W\{<matchparen>\}\;=\
	@{typedef struct tag$1 \{ $2 \} $1\;\n}
! ignore obsolete field ``_VARIANT_BOOL bool'' in union.
fields:VARIANT_BOOL <I>\;<space>_VARIANT_BOOL bool\;=@fields{VARIANT_BOOL $1\;}
! not supporting these union fields yet:
!fields:CA\J<K> <I>\;=\ \ \/\/ slot $2-value @tab{36}\:\: @type{CA$1}\;\n

! Don't re-export imported slots:
export-slot:\CcbSize=$0-value@end
export-slot:\Ccb=$0-value@end
export-slot:\CcBytes=$0-value@end
export-slot:\CdwPlatformId=$0-value@end
export-slot:\Cu=$0-value@end
export-slot:\CBuffer=$0-value@end
