
! Synopsis:  Additional translation rules specific to "winreg.h".
! Author:    David N. Gray
! Copyright: 1996, 1997, 1998 Functional Objects, Inc.  All rights reserved.


! insert copyright line into generated file:
ARGV:\N-module <G>\n=@set{heading;Module\:\ \ \ \ $1\n@copyright{\:}\n\n}

number:(\WHKEY\W)\W<number>=as(@type{HKEY},$1)@end

link:APIENTRY=, c-modifiers\: "__stdcall"@end

! special case: if ...A and ...W version have the same argument list,
! emit the ...A version only for now:
WINADVAPI <I> APIENTRY <L>\JA\W(<matchparen>)\;WINADVAPI <I> APIENTRY $2\JW\W(<matchparen>)\G\;=@{WINADVAPI $1 APIENTRY $2A($3)\;}!!!\n\/\* $2W omitted because arglist is same \*\/\n

! not used, not documented:
relevant-name:WIN31_CLASS=@fail

! special case arguments that look like output parameters but are not:
outok:lpcb\J<K0>=@fail! RegEnumKeyEx, RegEnumValue, etc. - input/output parm.
outok:lpReserved=@fail! always pass NULL, no value returned.

! Omit stuff that is only used for device drivers:
\#define PROVIDER_KEEPS_VALUE_LENGTH *\*PPROVIDER\;=

! Omit functions not in Windows 95 (and not likely to be used anyway):
relevant-function:InitiateSystemShutdown\J=@fail
relevant-function:AbortSystemShutdown\J=@fail

! Omit new function not yet defined in NT 4.0 (and not documented anyway):
relevant-function:RegOverridePredefKey\J=@fail
