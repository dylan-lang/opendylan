
! Additional translation rules for the portion of "winnt.h" to be
! included in the "win32-kernel" library.

! Copyright: 1998 Functional Objects, Inc.  All rights reserved.

! $HopeName$
! $Date: 2004/03/12 00:10:27 $

! Skip the part that has already been included in "win32-common":
\#ifndef\ _WINNT_\n<U>\n\#ifndef\ WIN32_NO_STATUS\n=

! Things to include:
relevant-name:STATUS_\J<I>=$0@end
relevant-name:FILE_\J<I>=$0@end
relevant-name:STANDARD_RIGHTS_\J<I>=$0@end
relevant-name:SECTION_\J<I>=$0@end
relevant-name:MUTANT_\J<I>=$0@end
relevant-name:READ_CONTROL\I=$0@end
relevant-name:SYNCHRONIZE\I=$0@end

! Things to exclude:
relevant-name:EXCEPTION_POINTERS=@fail
relevant-name:FILE_SEGMENT_ELEMENT\I=@fail
relevant-name:FILE_NOTIFY_INFORMATION\I=@fail


