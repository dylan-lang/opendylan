
! This file extends "patterns.pat" with rules specific to
! the X and Motif libraries.

! Copyright (C) 1998 Functional Objects, Inc. All rights reserved.

comment:\$\L<F>\:\W*\$=@out{\/\/\t$1\: $2\n}! RCS version info

\/\J\*\*\*\*\*\*\*\L*\n\ \* *\n\ \*\*\**\*\/=@export-end{}\N\/\/ $2

undef-flag:_NO_PROTO=@end
undef-flag:VMS=@end
undef-flag:CRAY=@end
undef-flag:__arm=@end
undef-flag:XM_1_1_BC=@end
undef-flag:IBM_MOTIF=@end
undef-flag:c_plusplus=@end
undef-flag:XTTRACEMEMORY=@end
undef-flag:offsetof=@end
undef-flag:SME=@end
undef-flag:XTSTRINGDEFINES=@end
undef-flag:XMSTRINGDEFINES=@end

def-flag:OSF_v1_2_4=@end
undef-flag:X_WCHAR=@end! suppress definition of wchar_t

! rules corresponding to macros defined in "Xfuncproto.h":
true-flag:NeedFunctionPrototypes=$0@end
false-flag:NeedVarargsPrototypes=$0@end! change to true-flag if Bug 4176 fixed.
relevant-function:XtVa\J<K1>\J<J><i>=@err{omitting function $0\n}@fail! remove when Bug 4176 fixed
false-flag:NeedNestedPrototypes=$0@end
false-flag:NeedWidePrototypes=$0@end
type:_Xconst <type>= \/\* const \*\/ $1@end
type:_XtConst_ <type>= \/\* const \*\/ $1@end
type:_XmConst <type>= \/\* const \*\/ $1@end
ctype:_Xconst\W=$0
ctype:_XtConst_\W=$0
ctype:_XmConst\W=$0

_XFUNCPROTOBEGIN=
_XFUNCPROTOEND=

type:register <type>= \/\* register \*\/ $1@end

externalref <type> <relevant-name>\;=\
	\Ndefine C-variable @export{$2} \:\: $1\n\
	\ \ c-name\: \"$2\"\;\nend\;\n

externalref <I> <I>\[\]\;=\
	\Ndefine C-variable @export{$2} \:\: @type{$1\*}\n\
	\ \ c-name\: \"$2\"\;\nend\;\n

extern\J<ref> <ctype> <relevant-name>\[\]\;=\
	\Ndefine C-variable @export{$3} \:\: @type{$2\*}\n\
	\ \ c-name\: \"$3\"\;\nend\;\n
ref:\Jalref\I=$0@end;\I=@end;=@fail

number:((char\*)\&<I>\[<number>\])=\
	\Ipointer-value-address(@map-name{$1}(), index\: $2)@end

! general case function declaration:
extern <type>\W<link>\W<relevant-function>\G(\W<fn-args>=\
	\N\ndefine inline-only C-function @export{$3}\n$4\N\
	@result{$1}\ \ c-name\: \"$3\"@wrap{$2}\;\n\
	end\;\n\
	@define{relevant-function\:@quote{$3}\=\@fail}! suppress duplicates
relevant-function::relevant-name

! some headers omit the return type for some functions; default to int
extern <relevant-function>\G(\W<fn-args>=\
	\N\ndefine inline-only C-function @export{$1}\n$2\N\
	@result{\/\* unspecified, defaults to \*\/ \<C-int\>}\
	\ \ c-name\: \"$1\"\;\n\
	end\;\n

relevant-function:int=@fail
extern <matchparen>\;=! skip non-relevant functions

! ugly hack to handle ");" inside conditional
fn-args:\)\W\;=@end
fn-args:\#if\ <false-flag><endline><matchcond>\)\;\
	\#else <args>\)\G\;\#endif<endline>=$4@end
fn-args:\A=@set{argnum;1}
fn-args::args

typelist:\#if\I\W<false-flag><endline><matchcond><elsepart>=@typelist{$4}
typelist:\#if\I\W<true-flag><endline><matchcond><elsepart>=@typelist{$3}

export:Opaque=$0! internal/obsolete; don't export

defn:_\J<I>\L <type><endline>=\
  define inline constant @export{@type{_$1}} \= $2\;\n@end

map-name:\A\_=\%
map-name:\IXK_\J<K>\I=XK-capital-$1
map-name:\IXK_\J<J1>\I=XK-small-$1
map-name:\IXK_\J<K1>\J<accent>\I=XK-capital-$1-$2
map-name:\IXK_\J<J1>\J<accent>\I=XK-small-$1-$2
accent:\Jgrave=$0@end
accent:\Jacute=$0@end
accent:\Jcircumflex=$0@end
accent:\Jtilde=$0@end
accent:\Jdiaeresis=$0@end
accent:\Jring=$0@end
accent:\Jcedilla=$0@end
accent:=@fail

! call-back function types
! [Note: the `c-name:' clause is intentionally omitted so that a unique
!   name will be generated for each expansion of the macro.]
typedef\ <type>(\W\*\W<relevant-name>)\G(\W<args>)\;=\N\n\
  define C-subtype @export{\<$2\>} ( \<C-function-pointer\> ) end\;\n\
  define macro @export{\<$2\>-callback-wrapper}\n\
  \ \{ \<@map-name{$2}\>-callback-wrapper(\?new\:name,\?old\:name) \} =>\n\
  \ \{ define C-callable-wrapper \?new of \?old\n\
       $3\
       @result{$1}\
  \ end C-callable-wrapper \}\n\
  end macro\;\n


! in "Xlib.h":
defn:Bool\L int<endline>=\
	\Ndefine inline-only constant @export{\<C-Bool\>} \= \<C-boolean\>\;\n\
	\Ndefine C-pointer-type @export{\<Bool\*\>} \=\> \<C-boolean\>\;\n\
	\Ndefine inline-only constant @export{\<Bool\>} \= \<boolean\>\;\n@end
defn:Status\L int<endline>=\
  define inline constant @export{\<C-Status\>} \= @type{int}\;\n\
  define inline constant @export{\<Status\*\>} \= @type{int\*}\;\n\
  define inline constant @export{\<Status\>} \= \<integer\>\;\n@end
resolve-type:Bool=C-$0
resolve-type:Status=C-$0

typedef X\J<I>\JEvent X\J<I>\JEvent\;=\N\
  define inline constant @export{@type{X$2Event}} \= @type{X$1Event}\;\n\
  define inline constant @export{@type{X$2Event\*}} \= @type{X$1Event\*}\;\n

! in "Intrinsic.h", don't export these:
defn:_Xt\J<I>\L <type>\N=\
	\Ndefine inline-only constant @type{_Xt$1} \= $2\;\n@end
type:_\J<I><stars>=\<_$1$2\>@end

! in "Intrinsic.h", these are given special handling:
type:String<stars>=\<X-String$1\>@end! <string> already defined in Dylan
type:Boolean<stars>=\<X-Boolean$1\>@end! <boolean> already defined in Dylan
typedef\ char Boolean\;=
resolve-type:Cardinal=C-$0
resolve-type:Dimension=C-$0
resolve-type:Position=C-$0
defn:XtCvt\J<I>\L XtCvt\J<I><endline>=\
  define inline constant @export{XtCvt$1} \:\: \<function\> \= XtCvt$2\;\n@end

type:Object<stars>=\<X-Object$1\>@end! <object> already defined in Dylan

! "opaque" structures in "Xlib.h" and "Intrinsic.h":
typedef\ struct <opaque>\W\{<matchparen>\}\W<relevant-name>\;=\
	\Ndefine C-subtype @export{@type{$3\*}} ( \<C-void\*\> ) end\;\n
typedef\ struct <opaque>\W\{<matchparen>\}\W\*\W<relevant-name>\;=\
	\Ndefine C-subtype @export{@type{$3}} ( \<C-void\*\> ) end\;\n
opaque:_XGC=$0@end
opaque:_XIM=$0@end
opaque:_XIC=$0@end
opaque:=@fail

! references to undefined structure types:
typedef\ struct _\J<I>\W\*\W<relevant-name>\;=\
	\Ndefine C-subtype @export{@type{$2}} ( \<C-void\*\> ) end\;\n\
	@define{scalar\:@quote{$2}\=\$0\@end}
! exception; this one is defined:
typedef\ struct\ _XtActionsRec\*XtActionList\;=\
  define inline constant @export{\<XtActionList\>} \= \<XtActionsRec\*\>\;\n

relevant-name:_XmString\J=@fail! only used in private headers
relevant-name:_XmStrings\I=$0@end! this one is OK

! function in struct slot
fields:<type>(\*<fn-type-name>)(<matchparen>)<space>\;=\
	\ \ sealed inline-only slot @export-slot{$2} \
		@tab{42}\:\: \<C-function-pointer\>\;\n

fn-type-name:\*=;<I>=$1;[<matchparen>]=;<space>=;=@fail

! special case too hairy to try to handle in general:
fields:struct <I>\*<I>,\*<I>\;=@fields{struct $1\* $2\; struct $1\* $3\;}

type:struct\ _XDisplay\W<stars>=\<Display$1\>@end! special case in Xlib.h
type:struct _\J<I>\W<stars>\W<Y0>=@resolve-type{$2$1}@end

! odd case in "Xlib.h":
args:char[<number>]<arg-id>=\ \ @putparm{$2,\<C-string\> \/\* char[$1] \*\/}

! function as function argument
args:<type>(\*<i>)(<matchparen>)=\ \ @putparm{$2,\<C-function-pointer\>}

args:struct _\J<I>\*<stars><arg-id>=\ \ @putparm{$3,\<$1\*$2\>}

! types in X.h:
! "Atom" does not need to be a "both" type:
typedef\ unsigned\ long\ Atom\;=\
  define inline constant @export{\<C-Atom\>} \= \<C-raw-unsigned-long\>\;\n\
  define inline constant @export{\<Atom\>} \= \<machine-word\>\;\n
number:(Atom)\W<number>=as(\<Atom\>,$1)@end
resolve-type:Atom=C-$0

typedef\ XID\ KeySym\;=\
  define inline constant @export{\<C-KeySym\>} \= \<C-unsigned-long\>\;\n\
  define inline constant @export{\<KeySym\>} \= \<integer\>\;\n
resolve-type:KeySym=C-$0

typedef\ unsigned\ long\ XID\;=\
  define inline constant @export{\<C-XID\>} \= \<C-raw-unsigned-long\>\;\n\
  define inline constant @export{\<XID\>} \= \<machine-word\>\;\n
resolve-type:XID=C-$0

! "Pixel" is an index so does not need to be a "both" type:
typedef\ unsigned\ long Pixel\;=\
  define inline constant @export{\<C-Pixel\>} \= \<C-unsigned-long\>\;\n\
  define inline constant @export{\<Pixel\*\>} \= \<C-unsigned-long\*\>\;\n\
  define inline constant @export{\<Pixel\>} \= \<integer\>\;\n
resolve-type:Pixel=C-$0

typedef\ unsigned\ long <relevant-name>\;=\
  define inline constant @export{\<C-$1\>} \= \<C-both-unsigned-long\>\;\n\
  define inline constant @export{\<$1\*\>} \= \<C-both-unsigned-long\*\>\;\n\
  define inline constant @export{\<$1\>} \= \<unsigned-32-bits\>\;\n\
  @define{resolve-type\:@quote{$1}\=C-\$0}\
  @define{scalar\:@quote{$1}\=\$0\@end}

typedef\ unsigned\ char XtEnum\;=\
  define inline constant @export{\<C-XtEnum\>} \= \<C-unsigned-char\>\;\n\
  define inline constant @export{\<XtEnum\>} \= limited(\<integer\>, min\: 0, max\: \#xFF)\;\n
resolve-type:XtEnum=C-$0

resolve-type:Window=C-$0
resolve-type:Drawable=C-$0
resolve-type:Font=C-$0
resolve-type:Pixmap=C-$0
resolve-type:Cursor=C-$0
resolve-type:Colormap=C-$0
resolve-type:GContext=C-$0
resolve-type:Mask=C-$0
resolve-type:VisualID=C-$0
resolve-type:Time=C-$0


! types in "Xm.h":
typedef\ long\ XmTextPosition\;=\
  define inline constant @export{\<C-XmTextPosition\>} \= \<C-long\>\;\n\
  define inline constant @export{\<XmTextPosition\>} \= \<integer\>\;\n
resolve-type:XmTextPosition=C-$0

typedef\ int\ <relevant-name>\;=\
  define inline constant @export{\<C-$1\>} \= \<C-int\>\;\n\
  define inline constant @export{\<$1\>} \= \<integer\>\;\n\
  define inline constant @export{\<$1\*\>} \= \<C-int\*\>\;\n\
  @define{resolve-type\:@quote{$1}\=C-\$0}\
  @define{scalar\:@quote{$1}\=\$0\@end}

\Nenum\W\{\W<enumbody>\}\G\W\;=\N$1\n@set{prev;-1}@export-end{}

! macros in "Xm.h":
defn:Xm\J<K1><J><I>\L Xm\J<K1><J><I><endline>=\
  \Ndefine inline-only constant @export{Xm$1$2$3} \=\
		@wrap{\s@map-name{Xm$4$5$6}\;}\n@end
relevant-name:XmVersion=@fail! conflicts with XmVERSION
number:Xm\J<K2>\J<i>=\$@map-name{$0}@end
number:Xm\J<K>\I=\$@map-name{$0}@end
symconst:_XmSDEFAULT_\J<K>\I=@map-name{$0}()@end

! for "XmStrDefs.h":
number:X\J/[tm][CNR]/\J<L>\I=\$@map-name{$0}@end
! suppress duplicate defines in "XmStrDefs.h":
defn:Xm\J<I>\L Xt\J$1\n\#define Xm\J$1 Xt\J$1\I=@defn{Xm$1 Xt$1\n}

! special case in "Xatom.h":
relevant-constant:XATOM_H=@fail

! for output parameters:
outok:database=@fail! for "Xresource.h"
outok:\/\*\W<outok>\W\*\/=$1@end
@undefine{outok\:\<I\>\=\$1\@end}! need to put the following rule first
outok:<I>\J_in_out=@fail
outok:list_return=$0@end
outok:list=@fail
outok:<I>\J_list\I=@fail
outok:<I>=$1@end
scalar:XtArgVal=$0@end
scalar:XtEnum=$0@end
scalar:Cardinal=$0@end
scalar:Dimension=$0@end
scalar:Position=$0@end
scalar:XtPointer=$0@end
scalar:XtTranslations=$0@end
scalar:XtAccelerators=$0@end
scalar:Modifiers=$0@end
scalar:Window=$0@end
scalar:Drawable=$0@end
scalar:Font=$0@end
scalar:Pixmap=$0@end
scalar:Cursor=$0@end
scalar:Colormap=$0@end
scalar:GContext=$0@end
scalar:KeySym=$0@end
scalar:Mask=$0@end
scalar:Atom=$0@end
scalar:VisualID=$0@end
scalar:Time=$0@end
scalar:KeyCode=$0@end
scalar:Widget=$0@end
scalar:XtValueMask=$0@end
scalar:XtIntervalId=$0@end
scalar:XtInputId=$0@end
scalar:XtWorkProcId=$0@end
scalar:XtGeometryMask=$0@end
scalar:XtGCMask=$0@end
scalar:Pixel=$0@end
scalar:XtCacheType=$0@end
scalar:XtCacheRef=$0@end
scalar:XtActionHookId=$0@end
scalar:char\*\*=$0@end
scalar:<I>\*\P\L*\J_return=$1\*@end

! special case for input-output parameters:
args:int\* \/\* <I>\J_in_out \*\/=\
	\ \ input output @putparm{$1_in_out,@type{int\*}}

! override to suppress warning
resolve-type:\*<stars><I>=@map-name{$2}\*$1

!
! Function-like macros

defn:<relevant-function>\J(<matchparen>)\W<defexpr>\n=\
	\N\ndefine @maybe-inline{$1} function @export{$1} ($2)\;\n\
	\ \ $3\nend\;\n@end
maybe-inline:DefaultVisual=@out{\/\/ not inline to work around Bug 4233}@end
maybe-inline:DefaultVisualOfScreen=@out{\/\/ not inline to work around Bug 4233}@end
maybe-inline:=inline-only@end
defexpr:\\\n\W=\S
defexpr:<I>(<expr-list>)=@map-name{$1}($2)
defexpr:<S>=$1
defexpr:\-\J\><I>=.@map-name{$1-value}
defexpr:.<I>=.@map-name{$1-value}
defexpr:(XtEnum)<number>=$1
defexpr:(unsigned)=
defexpr:(dpy)\-\>screens\[<defexpr>\]=\
	\Ipointer-value-address(dpy.screens-value, index\: $1)
defexpr:((<defexpr>)\W\&\W<number>)=logand($1, $2)
defexpr:(<J>)\W\<\<\W<number>=ash($1,$2)
defexpr:(\W<defexpr>)\<\<<number>=ash($1,$2)
defexpr:(<matchparen>)\W\>\>\W<D>=ash(@defexpr{$1},-$2)
defexpr:\=\J\=\W=\S\=\S
defexpr:\=\W=\S\:\=\S
defexpr:<J2>\J<L>\JClass\I=$0()! C global variable is Dylan function
defexpr:<J>\J<i>=\I$1$2
defexpr:(<J>)=\I$1
defexpr:(<type>)\W<defexpr>=c-type-cast($1, $2)
defexpr:FALSE=\#f
defexpr:TRUE=\#t
defexpr:<number>=$1
defexpr:(#)=(#)
defexpr:[#]=[#]
defexpr:\+\W<D>\I=\+ $1
defexpr:\>\J\=\W=\S$0\S
defexpr:\<\J\=\W=\S$0\S
defexpr:\>\W=\S$0\S
defexpr:\<\W=\S$0\S
defexpr:\&\J\&(<defexpr>)=\S\&@wrap{\s($1)}
defexpr:\|\J\|(<defexpr>)=\S\|@wrap{\s($1)}
defexpr:\&(<matchparen>[<matchparen>])=\
	\Ipointer-value-address(@defexpr{$1}, index\: @defexpr{$2})
defexpr:\&<I>[<defexpr>]=\Ipointer-value-address($1, index\: $2)
defexpr:\/\*<comment>\*\/=
! return to higher-level:
!defexpr:\P\)=@terminate
defexpr:\P\|=@terminate
defexpr:\P\<\<=@terminate
defexpr:\P\>\>=@terminate
defexpr:\"<string>\"=$0
! expressions not yet handled:
defexpr:\{=@fail
defexpr:\?=@fail
defexpr:\/=@fail
defexpr:\*=@fail
defexpr:\&=@fail
defexpr:\A\n=@fail
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

defn:_XtConst_\L *\n=@end
defn:_XmConst\L *\n=@end
number:\"<string>\"=$0@end

! for "X.h":
number:(int)\L\W<I>\W\N=@number{$1}@end


! Warn about macros that couldn't be handled:
defn:\L<relevant-function>\J(*\n=@err{Not translated\: \#define $0}@end
defn:\L<I>\J_H 1\n=@end
defn:\L<relevant-name> <G>*\n=@err{Not translated\: \#define $0}@end

! macros that can't be directly translated:
relevant-function:XtNumber=@fail
relevant-function:XtNew=@fail

! macros that are not functions:
relevant-function:externaldef=@fail

! macros translated by hand in "extra.dylan":
relevant-function:XtSetArg=@fail
relevant-function:XrmStringsEqual=@fail

! macro with undefined reference:
relevant-function:XtIsVendorShell=@fail

! macros that aren't applicable to Dylan:
relevant-name:NULL=@fail
relevant-name:externalref=@fail
relevant-name:FALSE=@fail
relevant-name:TRUE=@fail

