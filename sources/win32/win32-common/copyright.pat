
! defines function @copyright{} to insert copyright string with current year.
! Use @copyright{\:} for a colon after "Copyright" for use in a Dylan header.

copyright:\A<y>\Z=Copyright$1 @just-year{@date} Functional Objects, Inc\.\
		\ \ All rights reserved\.@end

just-year:<D4>=$1@end;?=
