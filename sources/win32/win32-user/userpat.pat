
! Additional translation rules for special cases in "winuser.h".

! special case to avoid compiler bug on machine-integer and multiple args:
number:(WS_POPUP\W\|\W<or-args>)=\%logior(@number{WS_POPUP}, $1)@end

\#if(WINVER\ \>\=\ 0x0400)\n\#define\ SBM_SETSCROLLINFO<matchcond>\#endif=\
	@{\#define\ SBM_SETSCROLLINFO $1}
\#if(WINVER\ \>\=\ 0x0400)\n\#define\ WM_NOTIFY<matchcond>\#endif=\
	@{\#define\ WM_NOTIFY $1}

relevant-constant:WH_MAX\J=@fail

stars:CONST=

! in WinHelp, this could take a pointer:
args:DWORD dwData=\ \ parameter dwData \:\: \<C-both-unsigned-long\>\;\n

! in SetWindowLong, this could take a full 32 bits:
args:LONG dwNewLong=\ \ parameter dwNewLong \:\: \<C-both-long\>\;\n

! sloppy use of conditialization:
args:\#if(<false-flag>)<matchcond>\#else<matchcond>\P)\G\;\n\#endif=@args{$3}


! avoid generating a redundant declaration:
typedef\ LPDLGITEMTEMPLATEA\ LPDLGITEMTEMPLATE\;=
typedef\ DLGITEMTEMPLATE\ \*LPDLGITEMTEMPLATEA\;=

WINUSERAPI\n\#ifndef <undef-flag><endline><matchcond><elsepart>\W<I>\W(<matchparen>)\;=@{WINUSERAPI $3 $5($6)\;}

! avoid a duplicate definition (there are actually two in the header file):
\#define\ MF_END 0x00000080L\L*Obsolete*\n=

! Doesn't quite fit usual pattern; redundant anyway:
typedef\ DLGTEMPLATE\ *LPDLGTEMPLATE\;\n\#endif=

! Can't use output parameters for these because they are arrays:
outok:lpList=@fail
outok:lpKids=@fail

relevant-name:SIZEZOOMSHOW\I=$0@end! not a wide version

! this function is declared but not defined or documented:
relevant-function:\IAlignRects\I=$0@fail
relevant-constant:\ICUDR_N\J<K>\I=$0@fail

! explicitly include so not filtered out as debugger support:
relevant-constant:SM_DEBUG\I=$0@end

