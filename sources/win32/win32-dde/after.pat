
! Additional special translation rules for "dde.h" and "ddeml.h"
!
! Copyright: 1997 Functional Objects, Inc.  All rights reserved.



\L\#define <relevant-constant> \"<string>\"<endline>=\
    define constant @export{\$$1} @tab{42}\=@wrap{\s\"$2\"\;}\n

fields:unsigned short <bit-fields>\;=$1

bit-fields:<I>\:<D><space>=\
	\ \ bitfield slot @export-slot{$1} @tab{32}\:\:\
	@wrap{\ @type{unsigned short}, width\: $2\;}\n
bit-fields:<I><space>=\
	\ \ slot @export-slot{$1} @tab{20}\:\: @type{unsigned short}\;\n
bit-fields:\,<space>=
bit-fields:\;=@end
bit-fields:=@fail

! don't export accessors for these filler slots:
export-slot:unused=$0@end
export-slot:reserved=$0@end
export-slot:fReserved=$0@end

! can't use `output parameter' for this:
outok:pidInst=@fail! DdeInitialize - input/output
