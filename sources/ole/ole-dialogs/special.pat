!! Additional translation rules for this particular file

! Copyright: 1997 Functional Objects, Inc.  All rights reserved.

! don't include Unicode versions of structures:
bad-struct:tag\J<K>\JW=$0@end

! don't export this inherited name:
export-slot:\ChInstance\I=$0-value@end

! not really a number, but needs to be allowed in a #define
number:TEXT("<string>"\G)=$0@end

! We do have property sheet support:
\N\#if(WINVER\>\=0x400)<endline><matchcond><elsepart>=@{$2}

def-flag:PSM_SETFINISHTEXTA=$0@end

! Use an output parameter for OleUIAddVerbMenu:
args:HMENU FAR \*lphMenu=@outparm{HMENU\* lphMenu}

! So type alias is not ignored as though a function alias:
\L\#define\ LP\J<K> LP\J$1\J/[AW]/<endline>=\
  define constant @export{\<LP$1\>} @tab{42}\=@wrap{\s\<LP$1$2\>\;}\n
