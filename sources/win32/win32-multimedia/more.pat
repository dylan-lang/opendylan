
! Additional special case rules for translating "mmsystem.h".
! This version works with the header files from the 
! MSDN "Platform SDK" of January 1998.
!
! $HopeName: D-win32-multi-media!more.pat(trunk.1) $
! $Date: 2004/03/12 00:10:28 $

@set-switch{t;1}@set-switch{w;1}


\N\/\*\*\*\*\*<U>\*\/=$0\n\n@export-end{}

type:struct <I>\J_tag\W<stars>\W<L0>=@resolve-type{$2$1}@end
stars:_huge=

! needed for struct self-references
ptr-type:wavehdr=LPWAVEHDR
ptr-type:midihdr=LPMIDIHDR

! special handling to avoid duplicate definition of "u-value" 
fields:union\{struct \{LONG lMinimum\;<space>LONG lMaximum\;<space>\}\; \
	    struct \{DWORD dwMinimum\;<space>DWORD dwMaximum\;<space>\}\; \
	    DWORD <I>[6]\;\}\W<I>\;=\
	\ \ sealed inline-only slot @export-slot{$6} @tab{42}\:\:\
		\ \<bounds-union\>\;\n

type:union\{struct \{LONG lMinimum\;<space>LONG lMaximum\;<space>\}\; \
	    struct \{DWORD dwMinimum\;<space>DWORD dwMaximum\;<space>\}\; \
	    DWORD <I>[6]\;\}\W=\<bounds-union\>@end

! These flags are tested but never defined or documented; assume undefined.
undef-flag:MCI_USE_OFFEXT=$0@end
undef-flag:_WIN32_VXD=$0@end

! omit obsolete Win16 functions:
false-flag:WINVER\<0x03\J<X2>\I=$0
false-flag:WINVER\<\=0x03\J<X2>\I=$0


number:(UINT)-1=\$FFFFFFFF@end
number:(MCIDEVICEID)-1=\$FFFFFFFF@end
number:((BYTE)0x\J<X2>)=\#x$1@end

! special cases to use machine-word arithmetic
number:(MIXER\J<I>\|\W<or-args>)=\%logior(@number{MIXER$1},$2)@end
number:MIXERCONTROL_CONTROLTYPE_\J<I>\W\+\W<number>=\
	u\%\+(@number{MIXERCONTROL_CONTROLTYPE_$1}, $2)@end

! This is already defined in the "win32-common" library:
typedef\ UINT\ FAR \*LPUINT\;=

! result codes are small integers so don't need a "both" type:
typedef\ UINT MMRESULT=\
	define inline constant @export{\<MMRESULT\>} \= \<C-unsigned-int\>\;\n
! count arguments don't need a "both" type:
args:UINT cb\J<I>=\ \ @putparm{cb$1,\<C-unsigned-int\>}

DECLARE_HANDLE(<L>)\;=\Ndefine C-subtype @export{\<$1\>} ( \<HANDLE\> ) end\;\n

typedef\ <I>(\WCALLBACK <I>)(<matchparen>)\G\;\W\
typedef $2 FAR\W\*\W<I>\;=\
  @define{ptr-type\:\\J@quote{$2}\=@quote{@map-name{$4}}}\
  @{typedef $1(CALLBACK\* $4)($3)\;}

typedef\ <K>\JCALLBACK <K>\; typedef $2 FAR\W\*LP\J$2\;=\
  \Ndefine inline constant @export{@type{LP$2}} \= @type{$1CALLBACK\*}\;\n\
  define macro @export{@type{LP$2}-callback-wrapper}\n\
  \t\{ @type{LP$2}-callback-wrapper(\?new:name,\?old:name) \} \=\>\n\
  \t\{ @type{LP$1CALLBACK}-callback-wrapper(\?new,\?old) \}\n\
  end\;\n

args:<scalar>\G\WFAR\W\*\W<outok>=@outparm{$1\*$2}

! Function-like macros

\L\#define <relevant-function>\J(<matchparen>) <defexpr>\n=\
	\N\ndefine inline-only function @export{$1} ($2)\;\n\ \ $3\nend\;\n
defexpr:\\\n\W=\S
defexpr:<I>(<expr-list>)=@map-name{$1}($2)
defexpr:<S>=$1
defexpr:\-\J\><I>=.@map-name{$1-value}
defexpr:((long)(<defexpr>) \& <number>)=\%logand($1, $2)
defexpr:((<defexpr>)\W\&\W<number>)=logand($1, $2)
defexpr:((long)sizeof(<J>)\&<number>)\<\<<number>=\
	ash(logand(size-of($1), $2), $3)
defexpr:(<J>)\W\<\<\W<number>=ash($1,$2)
defexpr:(\W<defexpr>)\<\<<number>=ash($1,$2)
defexpr:(DWORD)(BYTE)(<I>)\W\<\<\W<number>=ash(LOBYTE($1), $2)
defexpr:((BYTE)(<defexpr>\&0xFF))=logand($1, \#xFF)
defexpr:((BYTE)((<J>)\>\>16))=logand(HIWORD($1),\#xFF)
defexpr:(BYTE)(\W<defexpr>)=LOBYTE($1)
defexpr:(WORD)(\W<defexpr>)=LOWORD($1)
defexpr:((DWORD)\W<defexpr>\|\G\W<expr-or-args>)=logior($1, $2)
defexpr:((BYTE)\W<defexpr>\|\G\W<expr-or-args>)=logior(LOBYTE($1), $2)
defexpr:(DWORD)=
defexpr:(<matchparen>)\W\>\>\W<D>=ash(@defexpr{$1},-$2)
defexpr:BYTE=@fail;WORD=@fail;DWORD=@fail
defexpr:\=\=\W=\S\=\S
defexpr:\=\W=\S\:\=\S
defexpr:<J>\J<i>=\I$1$2
defexpr:(<J>)=\I$1
defexpr:<number>=$1
defexpr:(#)=(#)
defexpr:\/\*<comment>\*\/=
! return to higher-level:
!defexpr:\P\)=@terminate
defexpr:\P\|=@terminate
defexpr:\P\<\<=@terminate
defexpr:\P\>\>=@terminate
! expressions not yet handled:
defexpr:\{=@fail
defexpr:\?=@fail
defexpr:\/=@fail
defexpr:\*=@fail
defexpr:\&=@fail
defexpr:=@terminate
expr-list:\S=\S
expr-list:<defexpr>=$1
expr-list:,\W<defexpr>=,@wrap{\s$1}
expr-list:<G>=@err{line @line failed expr-list at\: $1\n}@fail

expr-or-args:\|=,
expr-or-args:\S=\I
expr-or-args:\\\n=\I
expr-or-args:(\W<defexpr>)=@wrap{\s$1}
expr-or-args:=@terminate

! hack to override rule in "patterns.pat" that ignores indented #define
\N\s\W\P\#define=

! Warn about macros that couldn't be handled:
\L\#define <relevant-function>\J(*\n=@err{Not translated\: $0}
