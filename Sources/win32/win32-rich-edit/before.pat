
! Synopsis:  Additional translation rules specific to "richedit.h".
! Author:    David N. Gray
! Copyright: 1996, 1997, 1998 Functional Objects, Inc.  All rights reserved.


! for Win32, the preprocessor expands this macro to "//":
fields:_WPAD\I*\n=

! fold these constants here since we can't be sure that the compiler will:
number:WM_USER\W\+\W<D>\I=\#x@radix{10;16;@add{1024;$1}}@end

! don't need general case of calling `logand' to mask the value:
number:((BYTE) 0x\J<X2>)=\#x$1@end

! these parameters could have values that need a <machine-word>:
fields:DWORD <both>\;=\
	\ \ slot @export-slot{$1} @tab{20}\:\: \<C-both-unsigned-long\>\;\n
both:dwMask=$0@end;dwEffects=$0@end;=@fail


! callback function types:
typedef\ <type>(<link>\G\W\*\W<relevant-name>)\G(\W<args>)\;=\N\n\
  define C-subtype @export{\<$3\>} ( \<C-function-pointer\> ) end\;\n\
  define macro @export{\<$3\>-callback-wrapper}\n\
  \ \{ \<$3\>-callback-wrapper(\?new\:name,\?old\:name) \} =>\n\
  \ \{ define C-callable-wrapper \?new of \?old\n\
       $4\
       @result{$1}\
     @cmps{$2;;;;\ \ @substring{2;999;$2}\;\n}\
  \ end C-callable-wrapper \}\n\
  end\;\n
link:=@end

type:HRESULT<stars>=\<C-both-long$1\> \/\* HRESULT \*\/ @end

! need special handling for `CF_...' constants:
\L\#define <relevant-constant> <string-value><endline>=\
	\Ndefine constant @export{\$$1} @tab{32}\=@wrap{\ \"$2\"\;\n}
!   in 1995 header file:
string-value:\"<string>\"=$1@end
!   in 1997 header file:
string-value:TEXT(\"<string>\")=$1@end
string-value:L\"<string>\"=$1@end
string-value:=@fail

! special case to override rule in "patterns.pat" that drops trailing 'A':
\L\#define\ RICHEDIT_CLASS\J<i> <string-value><endline>=\
  \Ndefine constant @export{\$RICHEDIT_CLASS$1} @tab{32}\=@wrap{\ \"$2\"\;\n}


! omit Unicode versions for now:
bad-struct:_<J>\Jw\I=$0@end
\L\#define <K> $1\JA<endline>=\N\
	define constant @export{@type{$1}} \= @type{$1A}\;\n\
	define constant @export{@type{LP$1}} \= @type{LP$1A}\;\n

! need to use <machine-word> for these:
number:(CFM_\J<I>\W\|<open-or-args>)=\%logior(@number{CFM_$1},$2)@end
number:(PFM_\J<I>\W\|<open-or-args>)=\%logior(@number{PFM_$1},$2)@end

! not handled by "patterns.pat" because not in parentheses:
number:CF\J<I>\W\|<open-or-args>=logior(@number{CF$1},$2)@end
open-or-args:<I>=\S@number{$1}
open-or-args:\|=,
open-or-args:\L\S=
open-or-args:\\\n=
open-or-args:<number>=\S$1
open-or-args:=@terminate

