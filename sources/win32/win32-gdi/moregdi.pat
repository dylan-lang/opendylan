
! Additional translation rules for "wingdi.h"

! Copyright: 1998 Functional Objects, Inc.  All rights reserved.

! The special rules below for the DC_... and DM_... constants are because
! these are duplicates of definitions in "windef.h", so are already defined
! in the `Win32-common' library.
\#define\ DC_FIELDS *\#define\ DC_COPIES\L *\n=
\#define DM_UPDATE *\#define\ DM_MODIFY\L *\n=
relevant-constant:DM_IN_\J<K>=@fail
relevant-constant:DM_OUT_\J<K>=@fail


! Omit functions for OpenGL defined in "opengl32.lib":
relevant-function:\Iwgl\J<K1><J><i>=@fail
relevant-constant:PFD_\J<K>=@fail
relevant-constant:WGL_SWAP\J=@fail


! Omit functions that are new in NT 5.0:
relevant-function:AlphaDIBBlend=@fail
relevant-function:TransparentDIBits=@fail
relevant-function:TransparentBlt=@fail
relevant-function:AlphaBlend=@fail
relevant-function:GradientFill=@fail

! misses general pattern because of the number:
relevant-function:GetTextExtentPoint32W\I=@fail

! Specify this to avoid multiple definitions:
undef-flag:NOTEXTMETRIC=$0@end

! Don't export obsolete names:
export:\<OLD\J*=@map-name{$0}

! Don't export inherited accessor (special case because no setter):
export:Buffer-value=$0

! omit because it refs a wide struct and is not used anyway:
bad-struct:tagEMRCREATECOLORSPACE\I=$0@end
