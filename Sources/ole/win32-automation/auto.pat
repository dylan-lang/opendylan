! Additional special rules for "oleauto.h"

! Copyright: 1997, 1998 Functional Objects, Inc.  All rights reserved.


link:STDAPICALLTYPE=, c-modifiers\: "__stdcall"@end

stars:HUGEP=

typedef struct FARSTRUCT <I>\W\{<matchparen>\}<matchparen>\;=\
	@{typedef struct $1\{$2\}$3\;}

! needed for BINDPTR in oaidl.h:
! old way (1995):
union tag\J<relevant-name>\W\{<matchparen>\}\G\;<space>\
	typedef union tag\J$1 <matchparen>\;=@{typedef union \{$2\} $4\;}
! new way (1997):
typedef union tag\J<I> __RPC_FAR\*<relevant-name>\;=\
	\Ndefine C-pointer-type @export{\<$2\>} \=\>\ @type{$1}\;\n

! needed for VARIANT in oaidl.h:
struct tag\J<relevant-name>\W\{<matchparen>\}\G\;=@{typedef struct \{$2\} $1\;}
! avoid generating:  define constant <VARIANT> = <VARIANT>;
typedef\ struct\ tag\J<K> $1\;=

! Ignore this problematic structure that will be handled specially:
typedef\ union\ tagCY \{<matchparen>\}=

! suppress duplicate definitions for these:
def-flag:_MEMBERID_DEFINED=@end
def-flag:_DISPID_DEFINED=@end

! allow unnamed union field:
fields:union\{<fields>\}\W<fieldname>\;=@incr{gencount}\
	@out{define C-union \<$2\%${gencount}\>\n$1\Nend\;\n}\
	\ \ slot @export-slot{$2} @tab{20}\:\: \<$2\%${gencount}\>\;\n
fieldname:\S=
fieldname:<I>=$1@end
fieldname:\#if(defined(NONAMELESSUNION))\W<I>\W\#endif\G=$1@end
fieldname:\Z=u@end

! function pointer as structure field:
fields:<type>(<link>\W<stars>\W<I>)(<matchparen>)\;=\
	\ \ slot @export-slot{$4} @tab{20}\:\: \<FARPROC\>\;\n


! Need to define enum type name as well as values:
typedef<space>enum tag\J<relevant-name>\W\{<matchparen>\}\G\W$2\;=\
	\N\/\/ enum $2\:\n@enumbody{$3}\N@set{prev;-1}@defint{$2}
typedef<space>enum tag\J<I>\W\{<matchparen>\}\G\W$2\;=
defint:<relevant-name>=define inline constant \<$1\> \= \<C-int\>\;\n
defint:*=



styps:__RPC_FAR=

! make this a subclass because not identical representation:
typedef<space>OLECHAR\ __RPC_FAR\ \*BSTR\;=\
	define C-subtype @export{\<BSTR\>} ( \<C-unicode-string\> ) end\;\n

! This one is not the same as without the "A":
fnname:\ALHashValOfNameSysA=$0

! for VARIANT_FALSE and VARIANT_TRUE in "oaidl.h":
number:(VARIANT_BOOL)\L0xffff\I=-1@end
number:(VARIANT_BOOL)0\I=0@end

! for "oleauto.h":
args:UINT <farptr><outok>=@outparm{$0}
args:DWORD <farptr><outok>=@outparm{$0}
args:long <farptr><outok>=@outparm{$0}
args:DOUBLE\* <outok>=@outparm{$0}
args:I\J<I> <farptr><farptr><outok>=@outparm{I$1\*\* $4}
args:void HUGEP\* <farptr><outok>=@outparm{void\*\* $2}
! special case for `DispInvoke':
args:void\ <farptr>\ _this=\ \ parameter this @tab{24}\:\: \<LPDISPATCH\>\;\n

farptr:FAR\W=$0
farptr:__RPC_FAR\W=$0
farptr:\*\W=$0@end
farptr:=@fail

outok:rgIndices=@fail! SafeArray... - vector pointer

type:char\ FAR\*<stars>=\<C-string\>$1@end
type:OLECHAR\W\*<stars>=\<C-unicode-string$1\>@end
type:DOUBLE<stars>=\<C-double$1\>@end

! to suppress warning from "win32-common/patterns.pat":
resolve-type:\*\*<K>\I=@map-name{$1}\*\*
resolve-type:\*wire\J<K>=@map-name{wire}\*

! for `SafeArrayAllocDescriptor' and `SafeArrayCopy':
args:SAFEARRAY <farptr><farptr><outok>=@outparm{SAFEARRAY\*\* $3}

typedef <K> <relevant-name>\W\;=\
	\Ndefine inline constant @export{@type{$2}} \= @type{$1}\;\n\
	@define{typedef\ $1 $2\\\;\=}! ignore duplicate definitions

! skip arglist of omitted functions (to avoid irrelevant warnings):
<I>(<matchparen>)\;=

