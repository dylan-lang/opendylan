
! Synopsis:  Load this file after "patterns.pat" to insert Functional Objects
!	     copyright into the header of the generated Dylan files.
! Author:    David N. Gray
! Copyright: 1996-2000 Functional Objects, Inc.  All rights reserved.

! include definition of `@copyright':
@define{@read{@mergepath{@inpath;copyright.pat;}}}

! insert copyright line into generated file:
ARGV:\N-module <G>\n=@set{heading;Module\:\ \ \ \ $1\n@copyright{\:}\n\n}
