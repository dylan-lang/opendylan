
! Synopsis:  Additional translation rules specific to "richedit.h".
! Author:    David N. Gray
! Copyright: 1996, 1997, 1998 Functional Objects, Inc.  All rights reserved.


! insert copyright line into generated file:
ARGV:\N-module <G>\n=@set{heading;Module\:\ \ \ \ $1\n@copyright{\:}\n\n}

! don't include code under ``#ifdef MAC'':
undef-flag:MAC=@end
undef-flag:MACPORT=@end

! already defined in "winuser":
def-flag:WM_CONTEXTMENU=@end
def-flag:EM_SCROLLCARET=@end
def-flag:WM_NOTIFY=@end

! Assume version 1.0 to retain compatibility with Windows 95:
def-flag:_RICHEDIT_VER=@end
true-flag:_RICHEDIT_VER\=\=0x0100=$0
false-flag:_RICHEDIT_VER\>\=0x02\J<X2>\I=$0

