!Synopsis:  Additional translation rules for "commctrl.h" and "prsht.h".
!Author:    David N. Gray
!Copyright: 1996, 1997, 1998 Functional Objects, Inc.  All rights reserved.

export-slot:DUMMYUNIONNAME\J<d>\I=@export-slot{u$1}@end

number:0U\W-\W<D>\J<optL>\I=-$1
number:(LP\J<K>)\W<signed-number>=make(\<LP$1\>, address\: $2)@end

! Fold these constant expressions during translation in order to 
! avoid generating IDVM names table entries.
number:WM_USER\W\+\W<D>\I=\#x@radix{10;16;@add{1024;$1}}@end
number:NM_FIRST-<D>\I=-$1@end
\#define\ <K>\J_FIRST 0x\J<X><endline>=\
  @define{number\:$1_FIRST\\W\+\\W\<D\>\\I\=\\\#x\@radix\{10\;16\;\@add\{@radix{16;10;$2}\;\$1\}\}\@end} \
  \Ndefine inline-only constant @export{\$$1-FIRST} @tab{57}\=\ \#x$2\;\n
\#define\ <K>\J_FIRST (0\J<optL>-<D><optL>)<endline>=\
  @define{number\:$1_FIRST\\W-\\W\<D\>\\I\=\@add\{-$3\;-\$1\}\@end} \
  \Ndefine inline-only constant @export{\$$1-FIRST} @tab{57}\=\ @right{4;-$3}\;\n

! Function-like macros

\L\#define <relevant-function>\J(<matchparen>) <defexpr>\n=\
  \N\ndefine @maybe-inline{$3}function @export{$1} ($2)\;\n\ \ $3\nend\;\n

maybe-inline:\Wwith-stack-structure=@end
maybe-inline:=inline-only @end

map-name:\ISNDMSG\I=SendMessage

defexpr:\\\n\W=\S
defexpr:<I>(<expr-list>)=@map-name{$1}($2)
defexpr:<S>=$1
defexpr:(int)=
defexpr:(INT)=
defexpr:(BOOL)\W<defexpr>=\~ zero\?($1)
defexpr:(WPARAM)(BOOL)<defexpr>=\
	select ( $1 ) \#t \=\> 1\; \#f \=\> 0\; end select
defexpr:(WPARAM)=
defexpr:(LPARAM)(<K>)NULL=\I0
defexpr:(LPARAM)(int)=
defexpr:(LPARAM)(UINT)(\W<pointer-type>)<defexpr>=\n\t\
	pointer-address(check-type($2, @type{$1}))
defexpr:(LPARAM)(\W<pointer-type>)\W<defexpr>=\n\t\
	pointer-address(check-type($2, @type{$1}))
defexpr:(LPARAM)(<J>)=c-type-cast(\<LPARAM\>, $1)
pointer-type:H\J<K>\J<i>\W\Z=$0@end
pointer-type:LP\J<I>=$0@end
pointer-type:PFNLVCOMPARE=$0@end
pointer-type:<I> FAR\*=$0@end
pointer-type:const <I> FAR\*=$0@end
pointer-type:=@fail
defexpr:(LPARAM)=
defexpr:(DWORD)=
defexpr:(UINT)=
defexpr:(COLORREF)=
defexpr:(H\J<K>)(<k>INT)<I>(<expr-list>)=c-type-cast(\<H$1\>, @map-name{$3}($4))
defexpr:(H\J<K>)<defexpr>=c-type-cast(@type{H$1}, $2)
defexpr:(void)<defexpr>=$1\;\n\ \ values()
defexpr:(<I>)<J0>=
defexpr:(<type>)(<J>\J<i>)=c-type-cast($1, $2$3)
defexpr:(<type>)<I>(<expr-list>)=c-type-cast($1, @map-name{$2}($3))
defexpr:(<type>)<I>=c-type-cast($1, $2)
defexpr:(<K>)\P(=
defexpr:(<J>)(<expr-list>)=$1($2)
defexpr:(<J>\J<i>)= $1$2
defexpr:(#)=(#)
defexpr:NULL=\$NULL-VOID
defexpr:-<-D0>=\S-\S
defexpr:+=\S+\S
defexpr:\s=\S
defexpr:(<J>)\W\<\<\W<number>=ash($1,$2)
defexpr:<J>\J<i>=\I$1$2
defexpr:_\J<J>\J<i>=$0
defexpr:<number>=$1
defexpr:\P,=@end
defexpr:\P\)=@end
defexpr:\{\W<I> _\J<I>\;<statements>\}=\
	with-stack-structure( $2 \:\: @type{LP$1} )\
	$3\N\tend with-stack-structure
defexpr:(LPARAM)(LV_ITEM FAR\*)\&_ms_lvi=ms_lvi.pointer-address
defexpr:(LPARAM)TVI_\J<I>=pointer-address(\$@map-name{TVI_$1})
defexpr:((<J>)\?(<defstmnt>)\G\W\:<defstmnt>)=\
	\Iif(\~ null-pointer\?($1))\n\t\t$2\n\telse $3\n\tend if
defexpr:\-\J\><I>=\.@map-name{$1}-value
defexpr:\=\==\=
defexpr:\==\:\=

! special cases to get correct type for NULL pointer:
defexpr:Animate_Open(<defexpr>, NULL)=Animate-Open($1, \$NULL-string)
defexpr:TreeView_GetNextItem(<defexpr>, NULL, <defexpr>)=\
	TreeView-GetNextItem($1, null-pointer(<HTREEITEM>), $2)
defexpr:CreateWindow(<defexpr>, NULL, <expr-list>)=\
	CreateWindow($1, \$NULL-string,@wrap{ $2})


! expressions not yet handled:
defexpr:\{=@fail
defexpr:\?=@fail
defexpr:\/=@fail
defexpr:\*=@fail

expr-list:\S=\S
expr-list:<defexpr>=$1
expr-list:,\W<defexpr>=,@wrap{\s$1}
expr-list:<G>=@err{failed expr-list at\: $1\n}@fail

defstmnt:\,=\;\n\t\t
defstmnt::defexpr

statements:_\J<I>.<I>\=<I>\;=\N\t$1.@map-name{$2}-value \:\= $3\;
statements:<defexpr>\;=\N\t$1\;
statements:\\\n\W=\S
statements:<G>=@err{failed statement at\: $1\n}@fail

! These aren't suitable for mechanical translation, so are handled
! specially in "special.dylan":
relevant-function:ListView_GetItemRect=@fail
relevant-function:ListView_SetItemPosition32=@fail
relevant-function:TreeView_GetItemRect=@fail
relevant-function:PropSheet_SetTitle=@fail
relevant-function:FORWARD_WM_NOTIFY=@fail

! include only the 8-bit version of structures
bad-struct:_\J<wide-name>=$0@end
bad-struct:tag\J<wide-name>=$0@end

! misses rule in "patterns.pat" because of '_':
relevant-function:ImageList_LoadImageW=@fail

\L\#define <constant-name> $1\JA<endline>=\
	\Ndefine inline constant @export{\$$1} \=@wrap{\s\$@map-name{$1}A\;}

\L\#define <type-name> <type-name><endline>=\
  \Ndefine inline constant @export{\<$1\>} \=@wrap{\s\<@map-name{$2}\>\;}\
  @define{map-name\:\\A$1\\Z\=@quote{@map-name{$2}}}

type-name:TB_\J<K>=@fail
type-name:SB_\J<K>=@fail
type-name:LV\J<K>=$0@end
type-name:TC\J<K>=$0@end
type-name:HD\J<K>=$0@end
type-name:TV\J<K>=$0@end
type-name:NM\J<K>=$0@end
type-name:TT\J<K>=$0@end
type-name:TB\J<K>=$0@end
type-name:TOOLBARCLASSNAME\J=@fail
type-name:TOOL\J<K>=$0@end
type-name:LPSTR_TEXT\J=@fail
type-name:<K2>\J_\J<K>=$0@end
type-name:LP\J<K><i>=$0@end
type-name:=@fail
constant-name:TB_\J<I>=$0@end
constant-name:SB_\J<I>=$0@end
constant-name:<K3>\J_\J<K>=$0@end
constant-name:<K>\J_CLASS=$0@end
constant-name:LP\J<K>=@fail
constant-name:LPSTR_TEXTCALLBACK=$0@end
constant-name:=@fail

@undefine{\\L\\\#define \<I\> \$1\\J\/[AW]\/\<endline\>\=}
! ignore function aliasing
\L\#define <K>\J<J>\J<i> $1$2$3\J/[AW]/<endline>=

! This is tricky because of the way #define is used to change names being
! subsequently defined.  map-name handles that as well as the conversion from
! C to Dylan, so we have to be careful to apply it at the right time.
do-typedef:LP\J<F>\d\G\<LP\J<F>\>=\N\
  @cmps{@map-name{LP$1};@map-name{LP$2};\
	define inline constant @export{\<@map-name{LP$1}\>} \= \<LP$2\>\;\n;;\
	define inline constant @export{\<@map-name{LP$1}\>} \= \<LP$2\>\;\n}

\#define <I>\JA <T>\n\G\
 \#define $1\JW <T>\n\G\
 \#ifdef UNICODE\W\
 \#define $1 $1\JW\W\
 \#else\W\
 \#define $1 $1\JA\W\
 \#endif=@{\#define $1 $2\n}

! window class names:
\L\#define <relevant-constant> \"<string>\"<endline>=\
	\Ndefine constant @export{\$$1} @tab{32} \=@wrap{\ \"$2\"\;}\n

! Avoid extra def due to these being in the reverse of the usual order.
\L\#define\ TOOLTIPS_CLASS TOOLTIPS_CLASSA=
\L\#define\ ANIMATE_CLASS ANIMATE_CLASSA=

! Don't include Unicode versions:
\L\#define\ <K>\JW <wide-name><endline>=
wide-name:\JVIEW\I=@fail
wide-name:\JW\I=W@end
wide-name:<I1>=$1
wide-name:=@fail

! duplicate type declaration in "prsht.h":
typedef\ struct\ _PROPSHEETPAGE\J<K1> FAR \*LPPROPSHEETPAGE\J$1\;=

! two 'W's confuses general rule for omitting wide versions
\L\#define\ NMTREEVIEWW NM_TREEVIEWW=
\L\#define\ NMTREEVIEW NMTREEVIEWA=\
	\Ndefine inline constant @export{\<NMTREEVIEW\>} \= \<NMTREEVIEWA\>\;




! pointers to undefined structures:
typedef struct _\J<K> <stars>\W<K>\;=\
	\Ndefine C-subtype @export{\<$3\>} ( \<C-void\*\> ) end\;\n

generic-args:\A=@bind{comma;}
generic-args:\Z=@unbind{comma}
generic-args:\S=
generic-args:output parameter *\;=
generic-args:<p>parameter <I> \:\: *\;=${comma}@wrap{ $2}@set{comma;\,}
generic-args:<G>=@err{line @line, failed match for generic-args\: $1\n}\
	@exit-status{1}

root-name:\APFN\J=;\ALPFN\J=;\J_\J=-

! allow output parameters in ImageList_GetIconSize:
scalar:int FAR=$0@end

relevant-name:LPFNPSPCALLBACKW=@fail
relevant-name:LPCPROPSHEET\J<K>\JW=@fail

! don't assume newer version of COMCTL32.DLL
false-flag:_WIN32_IE\>\=0x<X>=$0
true-flag:_WIN32_IE\<0x0400=$0
def-flag:_WIN32_IE=@end

! Don't seem to need to define this ugly thing:
def-flag:CCSIZEOF_STRUCT=@end
\L\#define\ <K>\J_V1_SIZE *\n=

! should have been under `#if (_WIN32_IE >= 0x0400)':
relevant-constant:SB_SETBKCOLOR\I=@fail

! Not needed here; defined in COM library:
def-flag:_HRESULT_DEFINED=@end

\L\#define <relevant-function>\J(*\n=@err{Not translated\: $0}
